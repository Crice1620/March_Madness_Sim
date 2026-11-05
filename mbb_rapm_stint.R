#!/usr/bin/env Rscript
# mbb_rapm_stint.R
# Real stint-based ridge RAPM from ESPN play-by-play substitutions (hoopR::load_mbb_pbp).
#
# 1. Starting fives come from the player box score (`starter`).
# 2. Substitution events ("X subbing in/out") are replayed to know the 10 players on the floor
#    between every pair of substitution times (a "stint").
# 3. Each stint is one row: y = home margin per 100 possessions, weight = possessions.
#    Home-team players enter as +1, away-team players as -1, plus an unpenalized home-court column.
#    Opponents are on the floor in the same row, so schedule strength is adjusted for directly.
# 4. Games whose reconstructed minutes don't match the box score are thrown away.
# 5. Seasons where too few games reconstruct cleanly are SKIPPED, leaving the box-score ridge
#    from mbb_rapm.R in place for that season (run mbb_rapm.R first, this script second).
#
# Output (overwrites): data/mbb/<season>/player_rapm.csv.gz
#   athlete_id, athlete_display_name, team_id, games_played, total_mins, rapm, season
#   rapm = points per 100 possessions better than an average D1 player while on the floor.
#
# Usage: Rscript mbb_rapm_stint.R [start_season] [end_season]

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(Matrix); library(glmnet); library(hoopR)
})

PREV_WEIGHT <- 0.5    # weight of last season's stints when fitting this season
NFOLDS      <- 5
MIN_KEEP    <- 0.60   # a season needs >= 60% of its games reconstructed cleanly
MAX_MIN_ERR <- 0.12   # per game: sum|reconstructed - box minutes| / sum box minutes
MIN_POSS    <- 0.5    # drop stints shorter than this many possessions

args <- commandArgs(trailingOnly = TRUE)
dirs <- list.dirs("data/mbb", full.names = FALSE, recursive = FALSE)
available <- sort(suppressWarnings(as.integer(dirs))); available <- available[!is.na(available)]
available <- available[file.exists(file.path("data/mbb", available, "games.csv.gz"))]
start_season <- if (length(args) >= 1 && nzchar(args[1])) as.integer(args[1]) else min(available)
end_season   <- if (length(args) >= 2 && nzchar(args[2])) as.integer(args[2]) else max(available)
seasons_to_run <- available[available >= start_season & available <= end_season]
message("Stint RAPM seasons: ", paste(seasons_to_run, collapse = ", "))

cache <- new.env()

get_stints <- function(szn) {
  key <- as.character(szn)
  if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache))
  res <- tryCatch(build_stints(szn), error = function(e) { message("  build_stints(", szn, ") failed: ", conditionMessage(e)); NULL })
  assign(key, res, envir = cache)
  res
}

build_stints <- function(szn) {
  gp <- file.path("data/mbb", szn, "games.csv.gz")
  if (!file.exists(gp)) return(NULL)
  games <- read_csv(gp, show_col_types = FALSE,
                    col_types = cols(game_id = col_character(), home_id = col_character(),
                                     away_id = col_character(), .default = col_guess())) %>%
    filter(!is.na(game_poss), game_poss > 0) %>%
    select(game_id, home_id, away_id, neutral, game_poss)

  # ---- box score: starters, team, minutes ----
  pb <- load_mbb_player_box(seasons = szn)
  if (is.null(pb) || nrow(pb) == 0) return(NULL)
  if (!"starter" %in% names(pb)) stop("player box has no 'starter' column")
  box <- pb %>%
    transmute(game_id = as.character(game_id), team_id = as.character(team_id),
              athlete_id = as.character(athlete_id), athlete_display_name,
              starter = as.logical(starter),
              minutes = suppressWarnings(as.numeric(gsub(":.*", "", as.character(minutes))))) %>%
    filter(!is.na(athlete_id), game_id %in% games$game_id) %>%
    distinct(game_id, team_id, athlete_id, .keep_all = TRUE)
  box$minutes[is.na(box$minutes)] <- 0

  # ---- play by play ----
  pbp <- load_mbb_pbp(seasons = szn)
  pbp <- pbp %>%
    transmute(game_id = as.character(game_id), n = game_play_number, type_text,
              text = tolower(text), aid = as.character(athlete_id_1),
              period = period_number,
              rem = as.numeric(clock_minutes) * 60 + as.numeric(clock_seconds),
              hs = as.numeric(home_score), aws = as.numeric(away_score)) %>%
    filter(game_id %in% games$game_id) %>%
    arrange(game_id, n)
  pbp$elapsed <- ifelse(pbp$period <= 2, (pbp$period - 1) * 1200 + (1200 - pbp$rem),
                        2400 + (pbp$period - 3) * 300 + (300 - pbp$rem))
  pbp <- pbp[!is.na(pbp$elapsed), ]

  pl_ids   <- unique(box$athlete_id)
  pid      <- setNames(seq_along(pl_ids), pl_ids)
  box_sp   <- split(box, box$game_id)
  pbp_sp   <- split(pbp, pbp$game_id)
  games_lu <- split(games, games$game_id)

  st <- c(total = 0, kept = 0, no_box = 0, bad_starters = 0, bad_minutes = 0, sub_unparsed = 0, sub_total = 0)
  out <- vector("list", length(pbp_sp)); gi <- 0L
  message(sprintf("  season %d: replaying %d games...", szn, length(pbp_sp)))

  for (gid in names(pbp_sp)) {
    st["total"] <- st["total"] + 1
    g <- games_lu[[gid]]; bx <- box_sp[[gid]]; ev <- pbp_sp[[gid]]
    if (is.null(g) || is.null(bx)) { st["no_box"] <- st["no_box"] + 1; next }

    on_h <- bx$athlete_id[bx$team_id == g$home_id & bx$starter %in% TRUE]
    on_a <- bx$athlete_id[bx$team_id == g$away_id & bx$starter %in% TRUE]
    if (length(on_h) != 5 || length(on_a) != 5) { st["bad_starters"] <- st["bad_starters"] + 1; next }
    team_of <- setNames(bx$team_id, bx$athlete_id)

    ev$hs  <- cummax(ifelse(is.na(ev$hs), 0, ev$hs))      # scores never decrease; fill gaps
    ev$aws <- cummax(ifelse(is.na(ev$aws), 0, ev$aws))
    t_end <- max(ev$elapsed)
    d_end <- ev$hs[nrow(ev)] - ev$aws[nrow(ev)]

    sub <- ev[ev$type_text == "Substitution" & !is.na(ev$aid), ]
    sub <- sub[order(sub$elapsed, sub$n), ]
    is_in  <- grepl("subbing in|enters", sub$text)
    is_out <- grepl("subbing out|leaves|exits", sub$text) & !is_in
    st["sub_total"]    <- st["sub_total"] + nrow(sub)
    st["sub_unparsed"] <- st["sub_unparsed"] + sum(!is_in & !is_out)
    grp <- split(seq_len(nrow(sub)), sub$elapsed)         # one group per substitution time

    rec <- setNames(numeric(nrow(bx)), bx$athlete_id)
    rows <- list(); k <- 0L
    t_prev <- 0; d_prev <- 0

    close_stint <- function(t, d) {
      dur <- t - t_prev
      if (dur > 0 && length(on_h) == 5 && length(on_a) == 5) {
        rec[c(on_h, on_a)] <<- rec[c(on_h, on_a)] + dur
        k <<- k + 1L
        rows[[k]] <<- c(pid[on_h], pid[on_a], dur, d - d_prev)
      }
    }

    for (ix in grp) {
      t <- sub$elapsed[ix[1]]; d <- sub$hs[ix[1]] - sub$aws[ix[1]]
      close_stint(t, d)
      for (a in sub$aid[ix][is_out[ix]]) { on_h <- setdiff(on_h, a); on_a <- setdiff(on_a, a) }   # outs first
      for (a in sub$aid[ix][is_in[ix]]) {
        tm <- unname(team_of[a]); if (is.na(tm)) next
        if (tm == g$home_id) on_h <- union(on_h, a) else if (tm == g$away_id) on_a <- union(on_a, a)
      }
      t_prev <- t; d_prev <- d
    }
    close_stint(t_end, d_end)

    err <- sum(abs(rec / 60 - bx$minutes)) / max(sum(bx$minutes), 1)
    if (k == 0 || err > MAX_MIN_ERR) { st["bad_minutes"] <- st["bad_minutes"] + 1; next }

    m <- do.call(rbind, rows)                              # 10 player ids, duration, home-margin change
    poss <- m[, 11] * g$game_poss / t_end                  # possessions in the stint
    gi <- gi + 1L; st["kept"] <- st["kept"] + 1
    out[[gi]] <- cbind(m[, 1:10, drop = FALSE], poss = poss, dmargin = m[, 12],
                       hca = 1 - as.numeric(isTRUE(g$neutral)), game_no = gi)
  }

  keep_frac <- st["kept"] / max(st["total"], 1)
  message(sprintf("  season %d: kept %d / %d games (%.0f%%) | no box %d | bad starters %d | minutes mismatch %d | subs unparsed %d / %d",
                  szn, st["kept"], st["total"], 100 * keep_frac, st["no_box"], st["bad_starters"],
                  st["bad_minutes"], st["sub_unparsed"], st["sub_total"]))
  if (gi == 0) return(list(stats = st, keep_frac = 0))

  mat <- do.call(rbind, out[seq_len(gi)])
  mat <- mat[mat[, "poss"] >= MIN_POSS, , drop = FALSE]

  box_sum <- box %>% filter(minutes > 0) %>%
    group_by(athlete_id) %>%
    summarise(athlete_display_name = dplyr::first(athlete_display_name),
              team_id = team_id[which.max(minutes)],
              games_played = n(), total_mins = sum(minutes), .groups = "drop")

  list(mat = mat, pl_ids = pl_ids, box_sum = box_sum, stats = st, keep_frac = keep_frac)
}

fit_season <- function(szn) {
  message(sprintf("\n=== Stint ridge RAPM %d ===", szn))
  cur <- get_stints(szn)
  if (is.null(cur) || is.null(cur$mat) || cur$keep_frac < MIN_KEEP) {
    message("  too few cleanly reconstructed games; keeping the existing (box-score) RAPM for this season")
    return(FALSE)
  }
  parts <- list(cur = list(m = cur$mat, pl = cur$pl_ids, wmult = 1))
  prev <- get_stints(szn - 1L)
  if (!is.null(prev) && !is.null(prev$mat) && prev$keep_frac >= MIN_KEEP)
    parts$prev <- list(m = prev$mat, pl = prev$pl_ids, wmult = PREV_WEIGHT)

  players <- sort(unique(unlist(lapply(parts, function(p) p$pl))))
  n_p <- length(players)
  I <- J <- X <- list(); y <- w <- hca <- gkey <- list(); off <- 0L
  for (nm in names(parts)) {
    p <- parts[[nm]]; n <- nrow(p$m)
    ids <- match(p$pl[p$m[, 1:10]], players)               # column-major: 10 columns of n
    I[[nm]]    <- rep(seq_len(n), times = 10) + off
    J[[nm]]    <- ids
    X[[nm]]    <- rep(c(rep(1, 5), rep(-1, 5)), each = n)
    y[[nm]]    <- p$m[, "dmargin"] / p$m[, "poss"] * 100
    w[[nm]]    <- p$m[, "poss"] * p$wmult
    hca[[nm]]  <- p$m[, "hca"]
    gkey[[nm]] <- paste0(nm, "_", p$m[, "game_no"])
    off <- off + n
  }
  y <- unlist(y); w <- unlist(w); hca <- unlist(hca); gkey <- unlist(gkey)
  n_rows <- length(y)
  Xs <- sparseMatrix(i = c(unlist(I), seq_len(n_rows)), j = c(unlist(J), rep(n_p + 1L, n_rows)),
                     x = c(unlist(X), hca), dims = c(n_rows, n_p + 1L))

  ug <- unique(gkey); set.seed(42)
  foldid <- sample(rep_len(seq_len(NFOLDS), length(ug)))[match(gkey, ug)]   # folds by GAME, not by stint

  message(sprintf("  fitting %d stints x %d players (cv.glmnet by game, %d folds)...", n_rows, n_p, NFOLDS))
  cv <- cv.glmnet(Xs, y, weights = w / mean(w), alpha = 0, foldid = foldid,
                  penalty.factor = c(rep(1, n_p), 0), standardize = FALSE,
                  nlambda = 30, lambda.min.ratio = 1e-4)
  b <- as.numeric(coef(cv, s = "lambda.min"))[-1]
  rapm <- b[seq_len(n_p)]
  message(sprintf("  lambda.min = %.5f | home-court = %.2f pts/100 | rapm sd = %.2f", cv$lambda.min, b[n_p + 1L], sd(rapm)))

  pl <- cur$box_sum %>%
    left_join(tibble(athlete_id = players, rapm = rapm), by = "athlete_id") %>%
    mutate(rapm = round(rapm, 3), season = szn)

  out_dir <- file.path("data/mbb", szn)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(pl, file.path(out_dir, "player_rapm.csv.gz"))

  top <- pl %>% filter(total_mins >= 400) %>% arrange(desc(rapm)) %>% head(10)
  message("  top players (>=400 min):")
  for (r in seq_len(nrow(top))) message(sprintf("    %-24s %6.2f  (%d g, %d min)", top$athlete_display_name[r], top$rapm[r], top$games_played[r], round(top$total_mins[r])))
  TRUE
}

for (s in seasons_to_run) {
  tryCatch(fit_season(s), error = function(e) message("FAILED season ", s, ": ", conditionMessage(e)))
  # free the season-before-last from the cache to keep memory flat
  rm(list = intersect(as.character(s - 2L), ls(cache)), envir = cache)
}
