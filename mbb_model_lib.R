# mbb_model_lib.R
# Shared by mbb_train.R, any daily prediction script, and the Shiny app.
# Keeping feature construction in ONE place guarantees training and prediction match.

# Per-team, as-of-game features produced by mbb_backfill.R (games_*.csv.gz has them as home_*/away_*;
# team_current_*.csv.gz has them unprefixed)
feat_cols <- c("gp", "adj_off_pre", "adj_def_pre", "adj_net_pre", "off_eff_pre", "def_eff_pre",
               "rec_off_pre", "rec_def_pre", "rec_margin_pre",
               "fg3a_rate_pre", "tov_rate_pre", "orb_rate_pre", "ftr_pre", "drb_rate_pre", "pace_pre",
               "road_margin_pre", "road_wp_pre", "sos_pre", "rest_days", "prior_net")

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
