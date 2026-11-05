#!/usr/bin/env Rscript
# mbb_app_data.R
# Runs in mbb_daily.yml AFTER mbb_rapm.R and mbb_backfill.R.
# Precomputes everything the Shiny app needs so the app only downloads small files:
#
#   data/mbb_app/teams.csv          team_id, abbrev, name, logo, conference
#   data/mbb_app/players.csv.gz     opponent-adjusted player values + style rates (+ rapm)
#   data/mbb_app/team_totals.csv    average game total per team (for the O/U line)
#   data/mbb_app/som_umatrix.csv    SOM grid cells (node, x, y, u) for the Team DNA map
#   data/mbb_app/team_nodes.csv     each team's SOM cell
#   data/mbb_app/app_meta.json      season, as_of, updated_at  (the app polls this to detect new data)

suppressPackageStartupMessages({
  library(hoopR); library(dplyr); library(readr); library(jsonlite); library(kohonen)
})

cur_year  <- as.integer(format(Sys.Date(), "%Y"))
cur_month <- as.integer(format(Sys.Date(), "%m"))
season <- if (cur_month >= 11) cur_year + 1L else cur_year

data_dir <- "data/mbb"
app_dir  <- "data/mbb_app"
dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

tc_path <- function(s) file.path(data_dir, s, "team_current.csv.gz")
# Early in a new season (before the first games exist) fall back to last season
if (!file.exists(tc_path(season))) season <- season - 1L
message("Exporting app data for season ", season)

read_tc <- function(p) {
  suppressWarnings(read_csv(p, show_col_types = FALSE,
                            col_types = cols(team_id = col_character(), .default = col_guess())))
}
tc <- read_tc(tc_path(season))

# ---- teams (id, abbrev, name, logo, conference) ----------------------------
conf <- tibble(team_id = character(), conference = character())
sched <- tryCatch(load_mbb_schedule(seasons = season), error = function(e) NULL)
if (!is.null(sched) && all(c("groups_is_conference", "groups_name", "home_id", "away_id") %in% names(sched))) {
  cs <- sched %>% filter(groups_is_conference %in% TRUE)
  conf <- bind_rows(
    transmute(cs, team_id = as.character(home_id), conference = groups_name),
    transmute(cs, team_id = as.character(away_id), conference = groups_name)
  ) %>%
    filter(!is.na(conference)) %>%
    count(team_id, conference) %>%
    group_by(team_id) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
    select(team_id, conference)
}
teams <- tc %>%
  select(team_id, team_abbrev, team_name) %>%
  left_join(conf, by = "team_id") %>%
  mutate(logo = paste0("https://a.espncdn.com/i/teamlogos/ncaa/500/", team_id, ".png"))
write_csv(teams, file.path(app_dir, "teams.csv"))

# ---- average game totals (for the over/under) -------------------------------
games_path <- file.path(data_dir, season, "games.csv.gz")
if (file.exists(games_path)) {
  games <- suppressWarnings(read_csv(games_path, show_col_types = FALSE,
                                     col_types = cols(home_id = col_character(), away_id = col_character(),
                                                      .default = col_guess())))
  team_totals <- bind_rows(
    transmute(games, team_id = home_id, total = home_score + away_score),
    transmute(games, team_id = away_id, total = home_score + away_score)
  ) %>%
    group_by(team_id) %>%
    summarise(avg_total = mean(total, na.rm = TRUE), n_games = n(), .groups = "drop")
} else {
  team_totals <- tibble(team_id = character(), avg_total = numeric(), n_games = integer())
}
write_csv(team_totals, file.path(app_dir, "team_totals.csv"))

# ---- players: opponent-adjusted value + style rates --------------------------
pb <- tryCatch(load_mbb_player_box(seasons = season), error = function(e) NULL)
players <- NULL
if (!is.null(pb) && nrow(pb) > 0) {
  num <- function(x) dplyr::coalesce(suppressWarnings(as.numeric(x)), 0)

  pv <- pb %>% transmute(
    game_id = as.character(game_id), athlete_id = as.character(athlete_id), athlete_display_name,
    team_id = as.character(team_id),
    minutes_num = suppressWarnings(as.numeric(gsub(":.*", "", as.character(minutes)))),
    fgm = num(field_goals_made), fga = num(field_goals_attempted),
    ftm = num(free_throws_made), fta = num(free_throws_attempted),
    fg3m = num(three_point_field_goals_made), fg3a = num(three_point_field_goals_attempted),
    pts = num(points), orb = num(offensive_rebounds), drb = num(defensive_rebounds),
    ast = num(assists), stl = num(steals), blk = num(blocks), tov = num(turnovers)
  ) %>% filter(!is.na(minutes_num), minutes_num > 0, team_id %in% tc$team_id)

  # opponent = the other team in the same game
  gt  <- pv %>% distinct(game_id, team_id)
  opp <- gt %>% rename(opp_id = team_id)
  pv  <- pv %>%
    inner_join(opp, by = "game_id", relationship = "many-to-many") %>%
    filter(team_id != opp_id, opp_id %in% tc$team_id)

  # adj_def_pre: LOWER = better defense. Flip so HIGHER = better (old app's def_rtg convention)
  def_lu <- tc %>% transmute(opp_id = team_id,
                             opp_def_rtg = -(adj_def_pre - mean(adj_def_pre, na.rm = TRUE)))
  pace_lu <- tc %>% transmute(team_id, team_pace = pace_pre)
  opp_adj_scale <- max(sd(def_lu$opp_def_rtg, na.rm = TRUE) * 3, 8)
  lg_pace <- mean(tc$pace_pre, na.rm = TRUE)

  pv <- pv %>%
    left_join(def_lu, by = "opp_id") %>% left_join(pace_lu, by = "team_id") %>%
    mutate(
      opp_def_rtg = coalesce(opp_def_rtg, 0),
      team_pace   = coalesce(team_pace, lg_pace),
      raw_val = pts + orb * 1.05 + drb * 0.65 + ast * 0.68 + stl * 1.72 + blk -
        tov * 1.41 - (fga - fgm) * 0.44 - (fta - ftm) * 0.54,
      val_adj = raw_val / team_pace * 100 * pmin(1.8, pmax(0.5, 1 + opp_def_rtg / opp_adj_scale))
    )

  # Every player who has appeared in at least 1 game is included.
  min_g <- 1L
  norm_ref <- function(v, m) {
    r <- median(v[m >= 20], na.rm = TRUE)
    if (!is.finite(r) || r == 0) r <- median(v, na.rm = TRUE)
    if (!is.finite(r) || r == 0) 1 else r
  }

  players <- pv %>%
    group_by(athlete_id, athlete_display_name, team_id) %>%
    summarise(
      games = n(), avg_minutes = mean(minutes_num), total_minutes = sum(minutes_num),
      avg_val_per100 = mean(val_adj),
      avg_pts = mean(pts), avg_reb = mean(orb + drb), avg_ast = mean(ast),
      avg_stl = mean(stl), avg_blk = mean(blk), avg_tov = mean(tov),
      tot_fga = sum(fga), tot_fgm = sum(fgm), tot_fta = sum(fta), tot_ftm = sum(ftm),
      tot_3pa = sum(fg3a), tot_3pm = sum(fg3m), tot_ast = sum(ast), tot_orb = sum(orb),
      tot_drb = sum(drb), tot_blk = sum(blk), tot_stl = sum(stl), tot_tov = sum(tov),
      .groups = "drop"
    ) %>%
    filter(games >= min_g) %>%
    mutate(
      weighted_val = avg_val_per100 * (avg_minutes / 40),
      fg_pct  = ifelse(tot_fga > 0, tot_fgm / tot_fga, NA_real_),
      ft_pct  = ifelse(tot_fta > 0, tot_ftm / tot_fta, NA_real_),
      fg3_pct = ifelse(tot_3pa > 0, tot_3pm / tot_3pa, NA_real_),
      p_3pa_rate = tot_3pa / pmax(tot_fga, 1),
      p_ast_rate = tot_ast / pmax(total_minutes, 1) * 40,
      p_orb_rate = tot_orb / pmax(tot_orb + tot_drb, 1),
      p_drb_rate = tot_drb / pmax(tot_orb + tot_drb, 1),
      p_blk_rate = tot_blk / pmax(total_minutes, 1) * 40,
      p_stl_rate = tot_stl / pmax(total_minutes, 1) * 40,
      p_ft_rate  = tot_fta / pmax(tot_fga, 1),
      p_tov_rate = tot_tov / pmax(total_minutes, 1) * 40,
      p_usage    = tot_fga / pmax(total_minutes, 1) * 40
    ) %>%
    mutate(weighted_val = weighted_val / norm_ref(weighted_val, avg_minutes)) %>%
    select(-starts_with("tot_"))

  rapm_p <- file.path(data_dir, season, "player_rapm.csv.gz")
  if (file.exists(rapm_p)) {
    rp <- suppressWarnings(read_csv(rapm_p, show_col_types = FALSE,
                                    col_types = cols(athlete_id = col_character(), .default = col_guess())))
    players <- left_join(players, rp %>% select(athlete_id, rapm), by = "athlete_id")
  } else players$rapm <- NA_real_

  if (nrow(players) > 0) {
    write_csv(players %>% mutate(across(where(is.double), ~ round(.x, 4))),
              file.path(app_dir, "players.csv.gz"))
  } else {
    message("0 qualifying players; keeping the previous players.csv.gz")
  }
} else {
  message("No player box yet; keeping the previous players.csv.gz")
}

# ---- SOM style map (trained on the last 5 season-end snapshots) ---------------
som_feats <- c("off_eff_pre", "def_eff_pre", "pace_pre", "fg3a_rate_pre",
               "tov_rate_pre", "orb_rate_pre", "ftr_pre", "drb_rate_pre")
snap <- bind_rows(lapply(seq(season - 4L, season), function(s) {
  p <- tc_path(s); if (file.exists(p)) read_tc(p) else NULL
})) %>% filter(if_all(all_of(som_feats), ~ !is.na(.x)))

mat <- as.matrix(snap[, som_feats])
mu  <- colMeans(mat); sdv <- apply(mat, 2, sd); sdv[sdv == 0] <- 1
set.seed(42)
som_model <- som(scale(mat, center = mu, scale = sdv),
                 grid = somgrid(xdim = 8, ydim = 8, topo = "hexagonal"),
                 rlen = 500, alpha = c(0.05, 0.01), radius = c(4, 1))

cur <- tc %>% filter(if_all(all_of(som_feats), ~ !is.na(.x)))
cur_scaled <- scale(as.matrix(cur[, som_feats]), center = mu, scale = sdv)
unit <- as.integer(predict(som_model, newdata = cur_scaled)$unit.classif)

pts   <- som_model$grid$pts
codes <- som_model$codes[[1]]
n_u   <- nrow(pts)
u <- numeric(n_u)
for (i in seq_len(n_u)) {
  d  <- sqrt(rowSums((pts - matrix(pts[i, ], nrow = n_u, ncol = 2, byrow = TRUE))^2))
  nb <- which(d > 0 & d < 2.1)
  if (length(nb) > 0)
    u[i] <- mean(sqrt(rowSums((codes[nb, , drop = FALSE] -
                                 matrix(codes[i, ], nrow = length(nb), ncol = ncol(codes), byrow = TRUE))^2)))
}
write_csv(tibble(node = seq_len(n_u), x = pts[, 1], y = pts[, 2], u = u),
          file.path(app_dir, "som_umatrix.csv"))
write_csv(tibble(team_id = cur$team_id, node = unit, node_x = pts[unit, 1], node_y = pts[unit, 2]),
          file.path(app_dir, "team_nodes.csv"))

# ---- meta (written LAST so the app never sees a half-updated set) -------------
write_json(list(
  season = season,
  as_of = as.character(max(tc$as_of, na.rm = TRUE)),
  updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  n_teams = nrow(tc),
  n_players = if (is.null(players)) NA else nrow(players)
), file.path(app_dir, "app_meta.json"), auto_unbox = TRUE, pretty = TRUE)

message("App data exported to ", app_dir)
