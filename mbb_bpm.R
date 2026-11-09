#!/usr/bin/env Rscript
# mbb_bpm.R
# Box-score BPM (Box Plus/Minus style) for every D1 player, built ONLY from the box score, so it
# exists for every season back to 2005. Replaces mbb_rapm.R (ridge on box minutes) and
# mbb_rapm_stint.R (play-by-play stints, only 2025+).
#
# Method (per season):
#   1. Per-game box value from linear weights (the same weights the app already uses for raw_val):
#        pts + 1.05 orb + 0.65 drb + 0.68 ast + 1.72 stl + blk - 1.41 tov - 0.44 missed FG - 0.54 missed FT
#      divided by the possessions the player was on the floor -> value per 100 possessions.
#   2. Centered on the minutes-weighted league mean (so 0 = average D1 player) and shrunk toward 0
#      with SHRINK_MIN pseudo-minutes (a 20-minute scrub with one big game is not a star).
#   3. Scaled with a regression of team strength on the team's minutes-weighted box value, so the
#      numbers are in "points per 100 possessions vs an average D1 player" units.
#   4. Team adjustment (what BPM does): team strength comes from an opponent-adjusted ridge rating
#      fit on this season's games (schedule strength handled here), and the gap between that and the
#      roster's box-score total is spread over the players in proportion to minutes, so that
#         sum(bpm_i * minutes_i / 200)  ==  team_net / 5          (exactly, per team)
#      i.e. five average-minute-weighted players at +x each make a team about +5x per 100.
#
# Output: data/mbb/<season>/player_bpm.csv.gz
#   athlete_id, athlete_display_name, team_id, games_played, total_mins, min_share, box_per100, bpm, season
#   bpm       = points per 100 possessions better than an average D1 player
#   min_share = share of the team's total minutes (sums to 1 per team)
#
# Needs data/mbb/<season>/games.csv.gz (game_id, home_id, away_id, neutral, home_margin, game_poss).
# Those columns do not depend on the roster feature, so BPM can be built before/after the backfill.
#
# Usage: Rscript mbb_bpm.R [start_season] [end_season]   (default: every season in data/mbb)

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(Matrix); library(glmnet); library(hoopR)
})

SHRINK_MIN <- 200   # pseudo-minutes pulling each player toward the average (about 6 games)
NFOLDS     <- 5

args <- commandArgs(trailingOnly = TRUE)
dirs <- list.dirs("data/mbb", full.names = FALSE, recursive = FALSE)
available <- sort(suppressWarnings(as.integer(dirs))); available <- available[!is.na(available)]
available <- available[file.exists(file.path("data/mbb", available, "games.csv.gz"))]
start_season <- if (length(args) >= 1 && nzchar(args[1])) as.integer(args[1]) else min(available)
end_season   <- if (length(args) >= 2 && nzchar(args[2])) as.integer(args[2]) else max(available)
seasons_to_run <- available[available >= start_season & available <= end_season]
message("BPM seasons: ", paste(seasons_to_run, collapse = ", "))

num <- function(x) dplyr::coalesce(suppressWarnings(as.numeric(x)), 0)

fit_season <- function(szn) {
  message(sprintf("\n=== Box-score BPM %d ===", szn))

  # ---- games (D1 vs D1, from the backfill) ----
  games <- read_csv(file.path("data/mbb", szn, "games.csv.gz"), show_col_types = FALSE,
                    col_types = cols(game_id = col_character(), home_id = col_character(),
                                     away_id = col_character(), .default = col_guess())) %>%
    filter(!is.na(home_margin), !is.na(game_poss), game_poss > 0) %>%
    transmute(game_id, home_id, away_id, neutral = dplyr::coalesce(as.numeric(neutral), 0),
              y = home_margin / game_poss * 100, w = game_poss, game_poss)
  if (nrow(games) < 50) { message("  fewer than 50 games; skipping"); return(FALSE) }

  # ---- team strength: opponent-adjusted ridge rating (per 100 possessions, 0 = average team) ----
  teams <- sort(unique(c(games$home_id, games$away_id))); n_t <- length(teams); n_g <- nrow(games)
  Xt <- sparseMatrix(i = rep(seq_len(n_g), 3),
                     j = c(match(games$home_id, teams), match(games$away_id, teams), rep(n_t + 1L, n_g)),
                     x = c(rep(1, n_g), rep(-1, n_g), 1 - games$neutral),
                     dims = c(n_g, n_t + 1L))
  set.seed(42)
  cv <- cv.glmnet(Xt, games$y, weights = games$w / mean(games$w), alpha = 0,
                  penalty.factor = c(rep(1, n_t), 0),          # home-court column unpenalized
                  standardize = FALSE, nfolds = NFOLDS, nlambda = 40, lambda.min.ratio = 1e-4)
  b <- as.numeric(coef(cv, s = "lambda.min"))[-1]
  team_net <- tibble(team_id = teams, net = b[seq_len(n_t)] - mean(b[seq_len(n_t)]))

  # ---- player box ----
  pb <- load_mbb_player_box(seasons = szn)
  if (is.null(pb) || nrow(pb) == 0) { message("  no player box; skipping"); return(FALSE) }
  pb <- pb %>%
    transmute(game_id = as.character(game_id), team_id = as.character(team_id),
              athlete_id = as.character(athlete_id), athlete_display_name,
              minutes = suppressWarnings(as.numeric(gsub(":.*", "", as.character(minutes)))),
              fgm = num(field_goals_made), fga = num(field_goals_attempted),
              ftm = num(free_throws_made), fta = num(free_throws_attempted),
              pts = num(points), orb = num(offensive_rebounds), drb = num(defensive_rebounds),
              ast = num(assists), stl = num(steals), blk = num(blocks), tov = num(turnovers)) %>%
    filter(!is.na(minutes), minutes > 0, !is.na(athlete_id)) %>%
    distinct(game_id, team_id, athlete_id, .keep_all = TRUE) %>%
    inner_join(games %>% select(game_id, home_id, away_id, game_poss), by = "game_id") %>%
    filter(team_id == home_id | team_id == away_id) %>%
    group_by(game_id, team_id) %>% mutate(team_min = sum(minutes)) %>% ungroup() %>%
    filter(team_min >= 150)                                      # drop games with missing minutes

  pb <- pb %>% mutate(
    game_len = team_min / 5,                                     # 40 (or 45, 50 with overtime)
    poss_on  = game_poss * minutes / game_len,                   # possessions he was on the floor for
    val = pts + orb * 1.05 + drb * 0.65 + ast * 0.68 + stl * 1.72 + blk -
          tov * 1.41 - (fga - fgm) * 0.44 - (fta - ftm) * 0.54)

  pt <- pb %>%
    group_by(athlete_id, team_id) %>%
    summarise(athlete_display_name = dplyr::first(athlete_display_name),
              games_played = n(), total_mins = sum(minutes),
              val = sum(val), poss = sum(poss_on), .groups = "drop") %>%
    filter(poss > 0, team_id %in% team_net$team_id) %>%
    mutate(val100 = val / poss * 100) %>%
    group_by(team_id) %>% mutate(M = total_mins / sum(total_mins)) %>% ungroup() %>%   # sums to 1 per team
    left_join(team_net, by = "team_id")

  # ---- center, shrink, scale, team-adjust ----
  mu <- weighted.mean(pt$val100, pt$total_mins)
  pt <- pt %>% mutate(rel = (val100 - mu) * total_mins / (total_mins + SHRINK_MIN))

  tm <- pt %>% group_by(team_id) %>%
    summarise(tv_raw = sum(M * rel), sm2 = sum(M^2), net5 = dplyr::first(net) / 5, .groups = "drop")
  s <- unname(coef(lm(net5 ~ tv_raw, data = tm))[2])             # team strength per unit of box value
  if (!is.finite(s) || s <= 0) s <- 1
  tm <- tm %>% mutate(delta = net5 - s * tv_raw)

  pt <- pt %>%
    left_join(tm %>% select(team_id, delta, sm2), by = "team_id") %>%
    mutate(bpm = s * rel + delta * M / sm2)                      # spread the team gap by minutes share

  message(sprintf("  %d games | %d teams | %d players | box->team slope %.2f | cor(box value, team net) = %.2f | bpm sd = %.2f",
                  n_g, n_t, nrow(pt), s, cor(tm$tv_raw, tm$net5), sd(pt$bpm)))

  # ---- one row per athlete (rare mid-season transfers: minutes-weighted across teams) ----
  out <- pt %>%
    group_by(athlete_id) %>%
    summarise(athlete_display_name = dplyr::first(athlete_display_name),
              team_id   = team_id[which.max(total_mins)],
              min_share = M[which.max(total_mins)],
              box_per100 = weighted.mean(val100, total_mins),
              bpm = weighted.mean(bpm, total_mins),
              games_played = sum(games_played),
              total_mins = sum(total_mins), .groups = "drop") %>%
    mutate(across(c(min_share, box_per100, bpm), ~ round(.x, 4)), season = szn) %>%
    select(athlete_id, athlete_display_name, team_id, games_played, total_mins,
           min_share, box_per100, bpm, season)

  out_dir <- file.path("data/mbb", szn)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(out, file.path(out_dir, "player_bpm.csv.gz"))

  top <- out %>% filter(total_mins >= 400) %>% arrange(desc(bpm)) %>% head(10)
  message("  top players (>=400 min):")
  for (r in seq_len(nrow(top)))
    message(sprintf("    %-24s %6.2f  (%d g, %d min)", top$athlete_display_name[r], top$bpm[r],
                    top$games_played[r], round(top$total_mins[r])))
  TRUE
}

for (s in seasons_to_run) {
  tryCatch(fit_season(s), error = function(e) message("FAILED season ", s, ": ", conditionMessage(e)))
}
