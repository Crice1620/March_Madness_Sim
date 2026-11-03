#!/usr/bin/env Rscript
# mbb_rapm.R
# Game-level ridge RAPM from box-score minutes (no lineup/stint data needed).
#
# One row per game:   y = home margin per 100 possessions
#   each player column = +minute_share*5 if on the home team, -minute_share*5 if on the away team
#   (minute_share*5 = "fraction of a full 5-man game this player was on the floor")
#   plus an unpenalized home-court column (0 for neutral games).
# Ridge shrinks every player toward 0 (= average D1 player); opponents are adjusted for automatically
# because opposing players enter with the opposite sign.
# The previous season's games are pooled in at half weight so returning players start with a real estimate.
#
# Output: data/mbb/<season>/player_rapm.csv.gz
#   athlete_id, athlete_display_name, team_id, games_played, total_mins, rapm, season
#   rapm = points per 100 possessions better than an average D1 player, over a full game on the floor.
#
# Usage: Rscript mbb_rapm.R [start_season] [end_season]   (default: every season in data/mbb, ascending)

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(Matrix); library(glmnet); library(hoopR)
})

PREV_WEIGHT <- 0.5   # weight of last season's games when fitting this season
NFOLDS      <- 5

args <- commandArgs(trailingOnly = TRUE)
dirs <- list.dirs("data/mbb", full.names = FALSE, recursive = FALSE)
available <- sort(suppressWarnings(as.integer(dirs))); available <- available[!is.na(available)]
start_season <- if (length(args) >= 1 && nzchar(args[1])) as.integer(args[1]) else min(available)
end_season   <- if (length(args) >= 2 && nzchar(args[2])) as.integer(args[2]) else max(available)
seasons_to_run <- available[available >= start_season & available <= end_season]
message("RAPM seasons: ", paste(seasons_to_run, collapse = ", "))

cache <- new.env()

# Game rows + long player-minute table for one season (cached so season N-1 isn't rebuilt)
get_rows <- function(szn) {
  key <- as.character(szn)
  if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache))
  res <- tryCatch(build_rows(szn), error = function(e) { message("  build_rows(", szn, ") failed: ", conditionMessage(e)); NULL })
  assign(key, res, envir = cache)
  res
}

build_rows <- function(szn) {
  gp <- file.path("data/mbb", szn, "games.csv.gz")
  if (!file.exists(gp)) return(NULL)
  games <- read_csv(gp, show_col_types = FALSE,
                    col_types = cols(game_id = col_character(), home_id = col_character(),
                                     away_id = col_character(), .default = col_guess())) %>%
    filter(!is.na(home_margin), !is.na(game_poss), game_poss > 0) %>%
    transmute(gkey = paste0(szn, "_", game_id), game_id, home_id, away_id,
              y = home_margin / game_poss * 100, w = game_poss,
              hca = 1 - as.numeric(neutral))

  pb <- load_mbb_player_box(seasons = szn)
  if (is.null(pb) || nrow(pb) == 0) return(NULL)
  pb <- pb %>%
    transmute(game_id = as.character(game_id), team_id = as.character(team_id),
              athlete_id = as.character(athlete_id), athlete_display_name,
              minutes = suppressWarnings(as.numeric(gsub(":.*", "", as.character(minutes))))) %>%
    filter(!is.na(minutes), minutes > 0, !is.na(athlete_id)) %>%
    distinct(game_id, team_id, athlete_id, .keep_all = TRUE) %>%
    group_by(game_id, team_id) %>% mutate(share = 5 * minutes / sum(minutes)) %>% ungroup()

  long <- pb %>%
    inner_join(games %>% select(game_id, gkey, home_id, away_id), by = "game_id") %>%
    mutate(sgn = case_when(team_id == home_id ~ 1, team_id == away_id ~ -1, TRUE ~ NA_real_)) %>%
    filter(!is.na(sgn)) %>%
    transmute(gkey, athlete_id, athlete_display_name, team_id, minutes, sgn, x = sgn * share)

  # keep only games where both teams have player rows
  ok <- long %>% group_by(gkey) %>% summarise(both = n_distinct(sgn) == 2, .groups = "drop") %>% filter(both)
  long  <- long %>% filter(gkey %in% ok$gkey)
  games <- games %>% filter(gkey %in% ok$gkey)
  message(sprintf("  season %d: %d games, %d player-game rows", szn, nrow(games), nrow(long)))
  list(games = games %>% select(gkey, y, w, hca), long = long)
}

fit_season <- function(szn) {
  message(sprintf("\n=== Ridge RAPM %d ===", szn))
  cur <- get_rows(szn)
  if (is.null(cur)) { message("  no data; skipping"); return(FALSE) }
  prev <- get_rows(szn - 1L)

  games <- cur$games; long <- cur$long
  if (!is.null(prev)) {
    games <- bind_rows(games, prev$games %>% mutate(w = w * PREV_WEIGHT))
    long  <- bind_rows(long, prev$long)
  }

  players <- sort(unique(long$athlete_id)); n_p <- length(players)
  i <- match(long$gkey, games$gkey); j <- match(long$athlete_id, players)
  n_g <- nrow(games)
  X <- sparseMatrix(i = c(i, seq_len(n_g)), j = c(j, rep(n_p + 1L, n_g)),
                    x = c(long$x, games$hca), dims = c(n_g, n_p + 1L))

  message(sprintf("  fitting %d games x %d players (cv.glmnet, %d folds)...", n_g, n_p, NFOLDS))
  set.seed(42)
  cv <- cv.glmnet(X, games$y, weights = games$w / mean(games$w), alpha = 0,
                  penalty.factor = c(rep(1, n_p), 0),      # home-court column is unpenalized
                  standardize = FALSE, nfolds = NFOLDS, nlambda = 60, lambda.min.ratio = 1e-5)
  b <- as.numeric(coef(cv, s = "lambda.min"))[-1]          # drop intercept
  rapm <- b[seq_len(n_p)]
  message(sprintf("  lambda.min = %.5f | home-court = %.2f pts/100 | rapm sd = %.2f",
                  cv$lambda.min, b[n_p + 1L], sd(rapm)))

  # only report players who appeared THIS season
  pl <- cur$long %>% group_by(athlete_id) %>%
    summarise(athlete_display_name = dplyr::first(athlete_display_name),
              team_id = team_id[which.max(minutes)],
              games_played = n(), total_mins = sum(minutes), .groups = "drop") %>%
    left_join(tibble(athlete_id = players, rapm = rapm), by = "athlete_id") %>%
    mutate(rapm = round(rapm, 3), season = szn)

  out_dir <- file.path("data/mbb", szn)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(pl, file.path(out_dir, "player_rapm.csv.gz"))

  top <- pl %>% filter(total_mins >= 400) %>% arrange(desc(rapm)) %>% head(8)
  message("  top players (>=400 min):")
  for (r in seq_len(nrow(top))) message(sprintf("    %-24s %6.2f  (%d g)", top$athlete_display_name[r], top$rapm[r], top$games_played[r]))
  TRUE
}

# ascending order so season N-1 rows are already cached when N is fit
for (s in seasons_to_run) {
  tryCatch(fit_season(s), error = function(e) message("FAILED season ", s, ": ", conditionMessage(e)))
}
