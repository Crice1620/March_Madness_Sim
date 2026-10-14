#!/usr/bin/env Rscript
# mbb_backfill.R
# Builds leak-free, as-of-game-date, opponent-adjusted team features for NCAA MBB.
# Usage: Rscript mbb_backfill.R <start_season> <end_season>
#   (season = ending year, e.g. 2025 = the 2024-25 season)
# Run seasons in ascending order in ONE process: season N reads season N-1's
# league means and final team ratings (prior_net) from disk.
#
# Outputs (data/mbb/):
#   games_<season>.csv.gz         one row per game: home_*/away_* pre-game features + home_margin
#   team_current_<season>.csv.gz  each team's feature state AFTER its last game (use for upcoming games)
#   league_<season>.csv           league-average style/efficiency (fallback prior for next season)

suppressPackageStartupMessages({
  library(hoopR); library(dplyr); library(tidyr); library(readr); library(zoo)
})

args <- commandArgs(trailingOnly = TRUE)
cur_year <- as.integer(format(Sys.Date(), "%Y")); cur_month <- as.integer(format(Sys.Date(), "%m"))
default_season <- ifelse(cur_month >= 11, cur_year + 1L, cur_year)
start_season <- if (length(args) >= 1) as.integer(args[1]) else default_season
end_season   <- if (length(args) >= 2) as.integer(args[2]) else start_season
out_dir <- "data/mbb"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Shrinkage strengths (pseudo-games pulling early-season values toward the league mean)
K_EFF <- 5; K_STYLE <- 5; K_SOS <- 5; ROLL_N <- 10

style_cols <- c("fg3a_rate", "tov_rate", "orb_rate", "ftr", "drb_rate", "pace")
league_cols <- c("off_eff", style_cols)
feat_cols <- c("gp", "adj_off_pre", "adj_def_pre", "adj_net_pre", "off_eff_pre", "def_eff_pre",
               "rec_off_pre", "rec_def_pre", "rec_margin_pre",
               paste0(style_cols, "_pre"),
               "road_margin_pre", "road_wp_pre", "sos_pre", "rest_days", "prior_net")

# ---- helpers ---------------------------------------------------------------

# Expanding mean over PRIOR games only (cumsum minus self), shrunk toward `prior`
shrunk_pre <- function(x, valid, prior, k) {
  v  <- as.numeric(valid)
  x0 <- ifelse(valid, x, 0)
  n_pre <- cumsum(v) - v
  s_pre <- cumsum(x0) - x0
  (s_pre + k * prior) / (n_pre + k)
}

add_pre <- function(df, col, prior_col, k, name = paste0(col, "_pre")) {
  df %>% group_by(team_id) %>%
    mutate(!!name := shrunk_pre(.data[[col]], is_real & !is.na(.data[[col]]), .data[[prior_col]], k)) %>%
    ungroup()
}

# League mean using only games on dates strictly before each row's date
# (blended with `fallback` so the first days of a season aren't noisy)
league_asof <- function(df, col, fallback, k0 = 200) {
  d <- df %>% filter(is_real, !is.na(.data[[col]])) %>%
    group_by(game_date) %>% summarise(s = sum(.data[[col]]), n = n(), .groups = "drop") %>%
    arrange(game_date) %>% mutate(cs = cumsum(s), cn = cumsum(n))
  idx <- findInterval(as.numeric(df$game_date - 1), as.numeric(d$game_date))
  cs <- c(0, d$cs); cn <- c(0, d$cn)
  (cs[idx + 1] + k0 * fallback) / (cn[idx + 1] + k0)
}

roll_pre <- function(x, n) {
  r <- zoo::rollapplyr(x, n, function(v) mean(v, na.rm = TRUE), partial = TRUE)
  r[is.nan(r)] <- NA_real_
  dplyr::lag(r)
}

# ---- main per-season builder ----------------------------------------------

build_season <- function(szn) {
  message("=== Season ", szn, " ===")
  tb <- load_mbb_team_box(seasons = szn)

  need <- c("game_id", "game_date", "team_id", "team_abbreviation", "team_display_name",
            "team_home_away", "team_score", "opponent_team_score", "field_goals_attempted",
            "offensive_rebounds", "defensive_rebounds", "turnovers", "free_throws_attempted",
            "three_point_field_goals_attempted")
  miss <- setdiff(need, names(tb))
  if (length(miss)) stop("team box missing columns: ", paste(miss, collapse = ", "))

  tb <- tb %>% transmute(
    game_id = as.character(game_id), game_date = as.Date(game_date),
    team_id = as.character(team_id), team_abbrev = team_abbreviation, team_name = team_display_name,
    home_away = tolower(team_home_away),
    score = as.numeric(team_score), opp_score = as.numeric(opponent_team_score),
    fga = as.numeric(field_goals_attempted), orb = as.numeric(offensive_rebounds),
    drb = as.numeric(defensive_rebounds), tov = as.numeric(turnovers),
    fta = as.numeric(free_throws_attempted), fg3a = as.numeric(three_point_field_goals_attempted)
  ) %>%
    filter(!is.na(score), !is.na(opp_score), !is.na(game_date)) %>%
    distinct(game_id, team_id, .keep_all = TRUE) %>%
    mutate(poss = fga - orb + tov + 0.475 * fta) %>%
    filter(is.finite(poss), poss > 0) %>%
    group_by(game_id) %>% filter(n() == 2) %>% ungroup()

  # D1 filter: teams in ESPN standings OR with >= 10 games (non-D1 opponents appear a handful of times)
  d1_std <- tryCatch(as.character(espn_mbb_standings(szn)$team), error = function(e) character())
  cnt <- tb %>% count(team_id)
  d1_ids <- union(cnt$team_id[cnt$n >= 10], tb$team_id[tb$team_name %in% d1_std])

  # Neutral-site flag from the schedule
  sched <- tryCatch(load_mbb_schedule(seasons = szn), error = function(e) NULL)
  neutral_tbl <- NULL
  if (!is.null(sched) && "neutral_site" %in% names(sched)) {
    gid <- intersect(c("game_id", "id"), names(sched))[1]
    neutral_tbl <- sched %>% transmute(game_id = as.character(.data[[gid]]),
                                       neutral = as.logical(neutral_site)) %>%
      distinct(game_id, .keep_all = TRUE)
  } else message("  (no neutral_site info found; treating all games as non-neutral)")

  opp <- tb %>% select(game_id, opp_id = team_id, opp_poss = poss, opp_orb = orb, opp_drb = drb)
  tg <- tb %>%
    inner_join(opp, by = "game_id", relationship = "many-to-many") %>%
    filter(team_id != opp_id, team_id %in% d1_ids, opp_id %in% d1_ids)
  if (!is.null(neutral_tbl)) tg <- left_join(tg, neutral_tbl, by = "game_id") else tg$neutral <- FALSE
  tg <- tg %>% mutate(
    neutral  = coalesce(neutral, FALSE),
    location = case_when(neutral ~ "neutral", home_away == "home" ~ "home", TRUE ~ "away"),
    avg_poss = (poss + opp_poss) / 2,
    off_eff  = score / avg_poss * 100,
    def_eff  = opp_score / avg_poss * 100,
    fg3a_rate = fg3a / pmax(fga, 1),
    tov_rate  = tov / poss,
    orb_rate  = orb / pmax(orb + opp_drb, 1),
    drb_rate  = drb / pmax(drb + opp_orb, 1),
    ftr       = fta / pmax(fga, 1),
    pace      = avg_poss,
    margin    = score - opp_score,
    win       = as.numeric(score > opp_score),
    is_real   = TRUE
  ) %>% select(game_id, game_date, team_id, team_abbrev, team_name, opp_id, location, score, opp_score,
               off_eff, def_eff, all_of(style_cols), margin, win, is_real)

  # Phantom "next game" row per team: its pre-game features == team state after all real games
  last_date <- max(tg$game_date)
  phantom <- tg %>% group_by(team_id) %>% slice(1) %>% ungroup() %>%
    select(team_id, team_abbrev, team_name) %>%
    mutate(game_id = paste0("phantom_", team_id), game_date = last_date + 1,
           opp_id = NA_character_, location = "home", is_real = FALSE)
  tg <- bind_rows(tg, phantom) %>% arrange(team_id, game_date, game_id)

  # League means as-of each date; fallback = previous season's league means (else this season's)
  own_mean <- sapply(league_cols, function(cn) mean(tg[[cn]][tg$is_real], na.rm = TRUE))
  prev_lg_path <- file.path(out_dir, sprintf("league_%d.csv", szn - 1))
  fb <- own_mean
  if (file.exists(prev_lg_path)) {
    pl <- read_csv(prev_lg_path, show_col_types = FALSE)
    fb[pl$metric] <- pl$value
  }
  for (cn in league_cols) tg[[paste0("lg_", cn)]] <- league_asof(tg, cn, fb[[cn]])
  tg$zero <- 0

  # Step A: expanding raw efficiency (prior games only)
  tg <- tg %>% group_by(team_id) %>% mutate(gp = cumsum(is_real) - as.numeric(is_real)) %>% ungroup()
  tg <- tg %>% add_pre("off_eff", "lg_off_eff", K_EFF) %>% add_pre("def_eff", "lg_off_eff", K_EFF)

  # Opponent's pre-game raw efficiency (same game_id -> opponent row)
  opp_tbl <- tg %>% select(game_id, opp_id = team_id, opp_off_pre = off_eff_pre, opp_def_pre = def_eff_pre)
  tg <- left_join(tg, opp_tbl, by = c("game_id", "opp_id"))

  # Opponent-adjusted single-game efficiencies: what the team did vs. an average opponent
  tg <- tg %>% mutate(
    adj_off_g = ifelse(is_real, off_eff - opp_def_pre + lg_off_eff, NA_real_),
    adj_def_g = ifelse(is_real, def_eff - opp_off_pre + lg_off_eff, NA_real_),
    adj_mar_g = adj_off_g - adj_def_g
  )

  # Step B: expanding adjusted ratings, style rates, recent form, road, rest
  tg <- tg %>% add_pre("adj_off_g", "lg_off_eff", K_EFF, "adj_off_pre") %>%
    add_pre("adj_def_g", "lg_off_eff", K_EFF, "adj_def_pre")
  for (cn in style_cols) tg <- add_pre(tg, cn, paste0("lg_", cn), K_STYLE)
  tg <- tg %>% mutate(adj_net_pre = adj_off_pre - adj_def_pre)

  tg <- tg %>% group_by(team_id) %>% mutate(
    rec_off_pre    = roll_pre(adj_off_g, ROLL_N),
    rec_def_pre    = roll_pre(adj_def_g, ROLL_N),
    rec_margin_pre = rec_off_pre - rec_def_pre,
    is_away   = as.numeric(is_real & location == "away"),
    road_n    = cumsum(is_away) - is_away,
    road_mg_s = cumsum(is_away * coalesce(margin, 0)) - is_away * coalesce(margin, 0),
    road_w_s  = cumsum(is_away * coalesce(win, 0)) - is_away * coalesce(win, 0),
    road_margin_pre = ifelse(road_n > 0, road_mg_s / road_n, NA_real_),
    road_wp_pre     = ifelse(road_n > 0, road_w_s / road_n, NA_real_),
    rest_days = coalesce(pmin(as.numeric(game_date - dplyr::lag(game_date)), 14), 7)
  ) %>% ungroup()

  # SOS: expanding mean of opponents' pre-game adjusted net rating
  net_tbl <- tg %>% select(game_id, opp_id = team_id, opp_net_pre = adj_net_pre)
  tg <- left_join(tg, net_tbl, by = c("game_id", "opp_id")) %>% add_pre("opp_net_pre", "zero", K_SOS, "sos_pre")

  # Preseason prior: last season's final adjusted net rating
  prev_cur_path <- file.path(out_dir, sprintf("team_current_%d.csv.gz", szn - 1))
  if (file.exists(prev_cur_path)) {
    pc <- read_csv(prev_cur_path, col_types = cols(team_id = col_character(), .default = col_guess()))
    tg <- left_join(tg, pc %>% select(team_id, prior_net = adj_net_pre), by = "team_id")
  } else tg$prior_net <- NA_real_

  # ---- outputs ----
  rnd <- function(df) df %>% mutate(across(where(is.double), ~ round(.x, 3)))

  current <- tg %>% filter(!is_real) %>%
    transmute(team_id, team_abbrev, team_name, as_of = last_date, across(all_of(feat_cols)))
  write_csv(rnd(current), file.path(out_dir, sprintf("team_current_%d.csv.gz", szn)))

  real <- tg %>% filter(is_real)
  home <- real %>% filter(location == "home" | (location == "neutral" & team_id < opp_id)) %>%
    transmute(game_id, game_date, season = szn, home_id = team_id, away_id = opp_id,
              home_abbrev = team_abbrev, neutral = as.numeric(location == "neutral"),
              home_score = score, away_score = opp_score, home_margin = margin,
              across(all_of(feat_cols))) %>%
    rename_with(~ paste0("home_", .x), all_of(feat_cols))
  away <- real %>% transmute(game_id, away_id = team_id, away_abbrev = team_abbrev,
                             across(all_of(feat_cols))) %>%
    rename_with(~ paste0("away_", .x), all_of(feat_cols))
  games <- inner_join(home, away, by = c("game_id", "away_id")) %>% arrange(game_date, game_id)
  write_csv(rnd(games), file.path(out_dir, sprintf("games_%d.csv.gz", szn)))

  write_csv(tibble(metric = league_cols, value = unname(own_mean)),
            file.path(out_dir, sprintf("league_%d.csv", szn)))

  chk <- games %>% filter(home_gp >= 10, away_gp >= 10)
  message(sprintf("  %d games, %d teams | corr(adj_net diff, margin | gp>=10) = %.3f | prior_net coverage %.0f%%",
                  nrow(games), nrow(current),
                  cor(chk$home_adj_net_pre - chk$away_adj_net_pre, chk$home_margin, use = "complete.obs"),
                  100 * mean(!is.na(current$prior_net))))
  invisible(TRUE)
}

failed <- c()
for (s in seq(start_season, end_season)) {
  ok <- tryCatch(build_season(s), error = function(e) { message("FAILED ", s, ": ", conditionMessage(e)); FALSE })
  if (!isTRUE(ok)) failed <- c(failed, s)
}
if (length(failed)) { message("Failed seasons: ", paste(failed, collapse = ", ")); quit(status = 1) }
