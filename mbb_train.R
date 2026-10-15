#!/usr/bin/env Rscript
# mbb_train.R
# Trains separate XGBoost models for Efficiency Margin and Total Possessions
# across historical NCAA MBB backfill datasets.

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(xgboost); library(jsonlite)
})

out_dir <- "data/mbb"
model_dir <- "models/mbb"
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

# Discover available backfilled seasons
mbb_dirs <- list.dirs(out_dir, full.names = FALSE, recursive = FALSE)
seasons <- sort(suppressWarnings(as.integer(mbb_dirs)))
seasons <- seasons[!is.na(seasons) & seasons != 2021] # exclude COVID season

message("Training seasons: ", paste(seasons, collapse = ", "))

# Load all historical game rows
games_list <- lapply(seasons, function(szn) {
  p <- file.path(out_dir, szn, "games.csv.gz")
  if (file.exists(p)) read_csv(p, show_col_types = FALSE) else NULL
})
all_games <- bind_rows(games_list)

if (nrow(all_games) == 0) {
  stop("No historical games found for training. Run mbb_backfill.R first.")
}

style_cols <- c("fg3a_rate", "tov_rate", "orb_rate", "ftr", "drb_rate", "pace")
feat_cols <- c("gp", "adj_off_pre", "adj_def_pre", "adj_net_pre", "off_eff_pre", "def_eff_pre",
               "rec_off_pre", "rec_def_pre", "rec_margin_pre",
               paste0(style_cols, "_pre"),
               "road_margin_pre", "road_wp_pre", "sos_pre", "rest_days", "prior_net",
               "roster_rapm")

# Build difference/sum features for matchup modeling
home_feats <- paste0("home_", feat_cols)
away_feats <- paste0("away_", feat_cols)

# Ensure all feature columns exist
missing_cols <- setdiff(c(home_feats, away_feats), names(all_games))
if (length(missing_cols) > 0) {
  stop("Missing features in games data: ", paste(missing_cols, collapse = ", "))
}

# Target calculations:
# 1. Efficiency Margin per 100 possessions = (home_margin / game_poss) * 100
# 2. Total Game Possessions = game_poss
training_df <- all_games %>%
  filter(!is.na(home_margin), !is.na(game_poss), game_poss > 0) %>%
  mutate(
    eff_margin = (home_margin / game_poss) * 100
  )

# Construct feature matrix (Home - Away differences for symmetry, plus game context like neutral site)
# We map each feature difference as Home minus Away
X_matrix_list <- lapply(feat_cols, function(fc) {
  training_df[[paste0("home_", fc)]] - training_df[[paste0("away_", fc)]]
})
names(X_matrix_list) <- paste0(feat_cols, "_diff")

X_df <- bind_cols(
  as_tibble(X_matrix_list),
  neutral = training_df$neutral,
  home_rest = training_df$home_rest_days,
  away_rest = training_df$away_rest_days
)

X <- as.matrix(X_df)

# Targets
y_em <- training_df$eff_margin
y_poss <- training_df$game_poss

message(sprintf("Training on %d total games with %d features.", nrow(X), ncol(X)))

# Train XGBoost Model 1: Efficiency Margin
dtrain_em <- xgb.DMatrix(data = X, label = y_em)
params_em <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

message("Training Efficiency Margin model...")
model_em <- xgb.train(params = params_em, data = dtrain_em, nrounds = 350, verbose = 0)
xgb.save(model_em, file.path(model_dir, "xgb_em.json"))

# Train XGBoost Model 2: Total Game Possessions (Pace)
dtrain_poss <- xgb.DMatrix(data = X, label = y_poss)
params_poss <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

message("Training Total Possessions model...")
model_poss <- xgb.train(params = params_poss, data = dtrain_poss, nrounds = 350, verbose = 0)
xgb.save(model_poss, file.path(model_dir, "xgb_poss.json"))

message("Models successfully trained and saved to ", model_dir, "/")
