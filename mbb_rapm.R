#!/usr/bin/env Rscript
# mbb_rapm.R
# Prior-informed RAPM (stint-based ridge) for NCAA MBB. DISPLAY ONLY - not a model feature.
# Usage: Rscript mbb_rapm.R [season]      (season = ending year)
#
# Reads : data/mbb/<season>/games.csv.gz      (game_id, game_poss; written by mbb_backfill.R)
#         data/mbb/<season>/player_bpm.csv.gz (athlete_id, bpm; written by mbb_bpm.R)
# Writes: data/mbb/<season>/player_rapm.csv.gz
#
# Method
#   1. Rebuild on-court lineups from ESPN play-by-play substitution events (starters from player box).
#   2. One row per stint: y = (home pts - away pts) per 100 poss, weight = estimated possessions.
#      X: +1 for the 5 home players, -1 for the 5 away players.
#   3. Ridge pulled toward the BPM prior instead of toward 0:
#         min  sum w (y - a - X b)^2 + lambda * ||b - b0||^2     with b0 = BPM
#      Implemented exactly via glmnet(alpha = 0, offset = X %*% b0), then b = b0 + delta.
#      Players with no BPM get prior 0 (average). lambda chosen by CV with folds grouped by game.
#   Output unit: points per 100 possessions vs an average player.

suppressPackageStartupMessages({
  library(hoopR); library(dplyr); library(readr); library(Matrix); library(glmnet)
})

# ---- config ----------------------------------------------------------------
PRIOR_SCALE <- 1     # lineup net rating ~ SUM of the 5 players' BPM, so 1 is the natural scale
N_FOLDS     <- 5
out_dir     <- "data/mbb"

args <- commandArgs(trailingOnly = TRUE)
cur_year <- as.integer(format(Sys.Date(), "%Y")); cur_month <- as.integer(format(Sys.Date(), "%m"))
szn <- if (length(args) >= 1) as.integer(args[1]) else ifelse(cur_month >= 11, cur_year + 1L, cur_year)
season_dir <- file.path(out_dir, szn)
message("=== RAPM season ", szn, " ===")

# ---- inputs ----------------------------------------------------------------
games_path <- file.path(season_dir, "games.csv.gz")
bpm_path   <- file.path(season_dir, "player_bpm.csv.gz")
if (!file.exists(games_path)) stop("missing ", games_path, " (run mbb_backfill.R first)")

games <- read_csv(games_path, show_col_types = FALSE,
                  col_types = cols(game_id = col_character(), .default = col_guess())) %>%
  select(game_id, game_poss) %>% filter(is.finite(game_poss), game_poss > 0) %>%
  distinct(game_id, .keep_all = TRUE)

bpm <- if (file.exists(bpm_path)) {
  read_csv(bpm_path, show_col_types = FALSE,
           col_types = cols(athlete_id = col_character(), .default = col_guess())) %>%
    select(athlete_id, bpm) %>% filter(!is.na(bpm)) %>% distinct(athlete_id, .keep_all = TRUE)
} else { message("No player_bpm file; prior = 0 for everyone"); tibble(athlete_id = character(), bpm = numeric()) }

pbp <- load_mbb_pbp(seasons = szn)
need_pbp <- c("game_id", "sequence_number", "type_text", "text", "period_number", "clock_display_value",
              "home_score", "away_score", "home_team_id", "athlete_id_1")
miss <- setdiff(need_pbp, names(pbp))
if (length(miss)) stop("pbp missing columns: ", paste(miss, collapse = ", "),
                       "\nAvailable: ", paste(names(pbp), collapse = ", "))

pb <- load_mbb_player_box(seasons = szn)
need_pb <- c("game_id", "team_id", "athlete_id", "athlete_display_name", "team_display_name", "minutes", "starter")
miss <- setdiff(need_pb, names(pb))
if (length(miss)) stop("player box missing columns: ", paste(miss, collapse = ", "))

pb <- pb %>% transmute(
  game_id = as.character(game_id), team_id = as.character(team_id), athlete_id = as.character(athlete_id),
  athlete_name = athlete_display_name, team_name = team_display_name,
  minutes = suppressWarnings(as.numeric(gsub(":.*", "", as.character(minutes)))),
  starter = as.logical(starter)
) %>% distinct(game_id, athlete_id, .keep_all = TRUE)

# ---- prepare events --------------------------------------------------------
clk <- as.character(pbp$clock_display_value)
clk_secs <- as.numeric(sub(":.*", "", clk)) * grepl(":", clk) * 60 + suppressWarnings(as.numeric(sub(".*:", "", clk)))
per <- as.integer(pbp$period_number)
elapsed <- ifelse(per <= 2, (per - 1) * 1200, 2400 + (per - 3) * 300) + ifelse(per <= 2, 1200, 300) - clk_secs

ev <- tibble(
  game_id = as.character(pbp$game_id), seq = suppressWarnings(as.numeric(pbp$sequence_number)),
  t = elapsed, hs = as.numeric(pbp$home_score), as = as.numeric(pbp$away_score),
  home_id = as.character(pbp$home_team_id), ath = as.character(pbp$athlete_id_1),
  txt = paste(pbp$type_text, pbp$text)
) %>%
  filter(game_id %in% games$game_id, !is.na(t), !is.na(hs), !is.na(as)) %>%
  mutate(kind = case_when(
    grepl("(?i)(enters the game|subbing in|subbed in|checks in|sub in)", txt, perl = TRUE)   ~ "in",
    grepl("(?i)(leaves the game|subbing out|subbed out|checks out|sub out)", txt, perl = TRUE) ~ "out",
    TRUE ~ NA_character_))

n_sub <- sum(!is.na(ev$kind))
message(sprintf("  %d pbp events in %d candidate games; %d substitution events detected",
                nrow(ev), length(unique(ev$game_id)), n_sub))
if (n_sub == 0) {
  message("No substitution events matched. Most common type_text values:")
  print(head(sort(table(pbp$type_text), decreasing = TRUE), 20))
  stop("Cannot rebuild lineups: update the in/out regex to match this season's pbp text")
}

# keep only sub events + the last event of each game (for final score / total time)
ev <- ev %>% arrange(game_id, t, seq) %>% group_by(game_id) %>%
  mutate(is_last = row_number() == n()) %>% ungroup() %>% filter(!is.na(kind) | is_last)

# ---- stint builder ----------------------------------------------------------
process_game <- function(e, bx) {
  hid <- e$home_id[1]
  st  <- bx[bx$starter %in% TRUE, ]
  h <- st$athlete_id[st$team_id == hid]; a <- st$athlete_id[st$team_id != hid]
  if (length(h) != 5 || length(a) != 5) return(NULL)
  team_of <- setNames(bx$team_id, bx$athlete_id)
  out <- vector("list", nrow(e)); k <- 0L
  lt <- 0; lh <- 0; la <- 0
  for (i in seq_len(nrow(e))) {
    t <- e$t[i]
    if (t > lt) {
      if (length(h) == 5 && length(a) == 5) {
        k <- k + 1L
        out[[k]] <- list(h = h, a = a, dur = t - lt, d = (e$hs[i] - lh) - (e$as[i] - la))
      }
      lt <- t; lh <- e$hs[i]; la <- e$as[i]
    }
    kd <- e$kind[i]
    if (!is.na(kd)) {
      p <- e$ath[i]
      if (is.na(p)) return(NULL)
      tm <- team_of[p]
      if (is.na(tm)) return(NULL)
      if (tm == hid) {
        if (kd == "out") { if (!p %in% h) return(NULL); h <- setdiff(h, p) } else h <- union(h, p)
      } else {
        if (kd == "out") { if (!p %in% a) return(NULL); a <- setdiff(a, p) } else a <- union(a, p)
      }
      if (length(h) > 6 || length(a) > 6) return(NULL)
    }
  }
  if (k == 0L) return(NULL)
  list(stints = out[seq_len(k)], total = max(e$t))
}

pb_split <- split(pb, pb$game_id)
ev_split <- split(ev, ev$game_id)
poss_lu  <- setNames(games$game_poss, games$game_id)

rows <- list(); n_ok <- 0L
for (gid in names(ev_split)) {
  bx <- pb_split[[gid]]
  if (is.null(bx)) next
  res <- process_game(ev_split[[gid]], bx)
  if (is.null(res) || res$total <= 0) next
  n_ok <- n_ok + 1L
  for (s in res$stints) {
    s$game <- gid; s$poss <- poss_lu[[gid]] * s$dur / res$total
    rows[[length(rows) + 1L]] <- s
  }
}
message(sprintf("  lineups reconstructed for %d / %d games (%.0f%%); %d stints",
                n_ok, length(ev_split), 100 * n_ok / max(length(ev_split), 1), length(rows)))
if (length(rows) < 1000) stop("Too few stints to fit RAPM")

# ---- design matrix ----------------------------------------------------------
n  <- length(rows)
hm <- do.call(rbind, lapply(rows, `[[`, "h"))[, 1:5, drop = FALSE]
am <- do.call(rbind, lapply(rows, `[[`, "a"))[, 1:5, drop = FALSE]
poss <- vapply(rows, `[[`, numeric(1), "poss")
dpts <- vapply(rows, `[[`, numeric(1), "d")
gam  <- vapply(rows, `[[`, character(1), "game")

keep <- poss > 0
players <- unique(c(hm, am))
X <- sparseMatrix(i = rep(seq_len(n), 10), j = match(c(hm, am), players),
                  x = rep(c(1, -1), each = 5 * n), dims = c(n, length(players)))
X <- X[keep, ]; poss <- poss[keep]; dpts <- dpts[keep]; gam <- gam[keep]
y <- dpts / poss * 100

# ---- prior-informed ridge ---------------------------------------------------
b0 <- PRIOR_SCALE * bpm$bpm[match(players, bpm$athlete_id)]
b0[is.na(b0)] <- 0
offs <- as.numeric(X %*% b0)

set.seed(42)
ug <- unique(gam)
fold <- sample(rep_len(seq_len(N_FOLDS), length(ug)))
foldid <- fold[match(gam, ug)]

message("  fitting ridge (", ncol(X), " players, ", nrow(X), " stints)...")
cv <- cv.glmnet(X, y, weights = poss, offset = offs, alpha = 0, standardize = FALSE,
                foldid = foldid, nlambda = 40)
cf    <- as.numeric(coef(cv, s = "lambda.min"))
hca   <- cf[1]
delta <- cf[-1]
rapm  <- b0 + delta

# ---- output -----------------------------------------------------------------
poss_on <- as.numeric(Matrix::t(abs(X)) %*% poss)

info <- pb %>% group_by(athlete_id, team_id) %>%
  summarise(athlete_name = first(athlete_name), team_name = first(team_name),
            minutes = sum(minutes, na.rm = TRUE), games = n(), .groups = "drop") %>%
  group_by(athlete_id) %>% slice_max(minutes, n = 1, with_ties = FALSE) %>% ungroup()

res <- tibble(athlete_id = players, rapm = rapm, bpm_prior = ifelse(is.na(bpm$bpm[match(players, bpm$athlete_id)]),
                                                                    NA_real_, b0),
              rapm_delta = delta, poss_on = poss_on) %>%
  left_join(info, by = "athlete_id") %>%
  transmute(athlete_id, athlete_name, team_id, team_name, games, minutes,
            poss_on = round(poss_on), bpm_prior, rapm, rapm_delta) %>%
  mutate(across(where(is.double), ~ round(.x, 3))) %>%
  arrange(desc(rapm))

write_csv(res, file.path(season_dir, "player_rapm.csv.gz"))
message(sprintf("  lambda.min = %.4g | home-court intercept = %.2f | corr(rapm, bpm_prior) = %.3f | %d players written",
                cv$lambda.min, hca, suppressWarnings(cor(res$rapm, res$bpm_prior, use = "complete.obs")), nrow(res)))
print(head(res %>% filter(poss_on >= 500) %>% select(athlete_name, team_name, minutes, bpm_prior, rapm), 10))
