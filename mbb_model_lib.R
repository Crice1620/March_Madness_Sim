# mbb_model_lib.R
# Shared by mbb_train.R, any daily prediction script, and the Shiny app.
# Keeping feature construction in ONE place guarantees training and prediction match.

# Per-team, as-of-game features produced by mbb_backfill.R (games_*.csv.gz has them as home_*/away_*;
# team_current_*.csv.gz has them unprefixed)
feat_cols <- c("gp", "adj_off_pre", "adj_def_pre", "adj_net_pre", "off_eff_pre", "def_eff_pre",
               "rec_off_pre", "rec_def_pre", "rec_margin_pre",
               "fg3a_rate_pre", "tov_rate_pre", "orb_rate_pre", "ftr_pre", "drb_rate_pre", "pace_pre",
               "road_margin_pre", "road_wp_pre", "sos_pre", "rest_days", "prior_net",
               "roster_bpm")

# h, a: data frames (same number of rows) with the feat_cols for the home / away team.
# neutral: 0/1 scalar or vector. Returns the model's feature data frame.
matchup_features <- function(h, a, neutral) {
  cols <- setdiff(feat_cols, "gp")
  out <- lapply(cols, function(cn) h[[cn]] - a[[cn]])
  names(out) <- paste0(cols, "_diff")
  out <- as.data.frame(out)
  out$min_gp     <- pmin(h$gp, a$gp)            # how much to trust the in-season numbers
  out$pace_avg   <- (h$pace_pre + a$pace_pre) / 2
  out$is_neutral <- as.numeric(neutral)
  out
}

# Predicted home margin. For neutral games "home" is arbitrary, so average both orientations.
predict_margin <- function(model, feature_names, h, a, neutral = FALSE) {
  f <- function(x, y) {
    m <- as.matrix(matchup_features(x, y, as.numeric(neutral))[, feature_names, drop = FALSE])
    predict(model, xgboost::xgb.DMatrix(m))
  }
  if (isTRUE(as.logical(neutral))) (f(h, a) - f(a, h)) / 2 else f(h, a)
}

# Model-based team rating: expected margin vs. an average D1 team on a neutral floor.
# team_current: rows from team_current_<season>.csv.gz
model_ratings <- function(model, feature_names, team_current) {
  tc  <- as.data.frame(team_current)
  avg <- as.data.frame(lapply(tc[feat_cols], function(x) mean(x, na.rm = TRUE)))
  avg <- avg[rep(1, nrow(tc)), , drop = FALSE]
  tc$rating <- predict_margin(model, feature_names, tc[feat_cols], avg, neutral = TRUE)
  tc[order(-tc$rating), c("team_id", "team_abbrev", "team_name", "rating")]
}

# ---- Team valuation (minute-weighted player value) --------------------------------------------
# Team Value = sum_i( Player Value_i x M_i ),  M_i = minutes_i / 200   (40-minute game, 5 x 40 = 200)
#   value   : player BPM
#   minutes : the player's minutes per TEAM game (an injured/out player is simply given 0 via `out`)
#   out     : logical, TRUE = injured / out of the rotation -> M_i = 0, so the team value drops by that
#             player's share and is NOT re-normalised (the missing minutes count as an average, 0-BPM player).
# The result is a minutes-weighted average BPM. Multiply by 5 to read it as points per 100 possessions
# better than an average D1 team (five players on the floor).
team_value <- function(value, minutes, out = FALSE, game_minutes = 200) {
  m <- dplyr::coalesce(as.numeric(minutes), 0) / game_minutes
  m <- m * (!as.logical(out))
  sum(dplyr::coalesce(as.numeric(value), 0) * m)
}

# Shift a team's roster_bpm feature (from team_current) when some players are out:
# removes exactly their value x minute-share, leaving everyone else's share where it was.
roster_bpm_after_outs <- function(base_roster_bpm, value, minutes, out) {
  base_roster_bpm - (team_value(value, minutes) - team_value(value, minutes, out = out))
}
