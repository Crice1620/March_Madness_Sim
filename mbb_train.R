#!/usr/bin/env Rscript
# mbb_train.R
# Trains the single XGBoost margin model the Shiny app loads, using the SAME feature code as the app
# (mbb_model_lib.R: feat_cols + matchup_features). Usage: Rscript mbb_train.R [first_season] [last_season]
#
# Writes (all in data/mbb_model/, which is what the app downloads and what the workflows commit):
#   xgb_model.json          the model (target = home margin in points)
#   meta.json               feature_names, resid_sd, loso_rmse, nrounds, seasons, ...
#   feature_importance.csv  Feature, Gain, Cover, Frequency

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(xgboost); library(jsonlite)
})

source("mbb_model_lib.R")   # feat_cols, matchup_features()

out_dir   <- "data/mbb"
model_dir <- "data/mbb_model"
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

# ---- seasons ---------------------------------------------------------------
mbb_dirs <- list.dirs(out_dir, full.names = FALSE, recursive = FALSE)
seasons  <- sort(suppressWarnings(as.integer(mbb_dirs)))
seasons  <- seasons[!is.na(seasons) & seasons != 2021]   # exclude COVID season

# Blank / missing last_season = most recent COMPLETED season (from June on, this calendar year's).
args <- commandArgs(trailingOnly = TRUE)
arg_int <- function(i) if (length(args) >= i && nzchar(trimws(args[i]))) suppressWarnings(as.integer(args[i])) else NA_integer_
cur_year  <- as.integer(format(Sys.Date(), "%Y"))
cur_month <- as.integer(format(Sys.Date(), "%m"))
first_season <- arg_int(1); if (is.na(first_season)) first_season <- min(seasons)
last_season  <- arg_int(2); if (is.na(last_season))  last_season  <- if (cur_month >= 6) cur_year else cur_year - 1L
seasons <- seasons[seasons >= first_season & seasons <= last_season]
if (length(seasons) == 0) stop("No season folders in ", out_dir, " between ", first_season, " and ", last_season)
message("Training seasons: ", paste(seasons, collapse = ", "))

# ---- load games ------------------------------------------------------------
all_games <- bind_rows(lapply(seasons, function(szn) {
  p <- file.path(out_dir, szn, "games.csv.gz")
  if (file.exists(p)) read_csv(p, show_col_types = FALSE) else NULL
}))
if (nrow(all_games) == 0) stop("No historical games found. Run mbb_backfill.R first.")

need <- c(paste0("home_", feat_cols), paste0("away_", feat_cols), "home_margin", "neutral", "season")
missing_cols <- setdiff(need, names(all_games))
if (length(missing_cols) > 0) stop("Missing columns in games data: ", paste(missing_cols, collapse = ", "))

games <- all_games %>% filter(!is.na(home_margin))

# ---- features: exactly what the app builds at prediction time ---------------
strip <- function(df, prefix) {
  out <- df[paste0(prefix, feat_cols)]
  names(out) <- feat_cols
  out
}
X_df <- matchup_features(strip(games, "home_"), strip(games, "away_"), games$neutral)
feature_names <- names(X_df)
X <- as.matrix(X_df)
y <- games$home_margin

# Games from a neutral site: "home" is arbitrary, so mirror them (swap sides, negate margin)
# so the model cannot learn a home-court edge from an arbitrary label.
neut <- which(games$neutral == 1)
if (length(neut) > 0) {
  Xm <- as.matrix(matchup_features(strip(games[neut, ], "away_"), strip(games[neut, ], "home_"), 1))
  X <- rbind(X, Xm[, feature_names, drop = FALSE])
  y <- c(y, -games$home_margin[neut])
  season_vec <- c(games$season, games$season[neut])
} else season_vec <- games$season

message(sprintf("Training on %d rows (%d games) with %d features.", nrow(X), nrow(games), ncol(X)))

# ---- leave-one-season-out CV to pick nrounds and estimate the error -----------
params <- list(objective = "reg:squarederror", eval_metric = "rmse",
               max_depth = 4, eta = 0.05, subsample = 0.8, colsample_bytree = 0.8,
               min_child_weight = 20)

dall <- xgb.DMatrix(data = X, label = y)
folds <- lapply(sort(unique(season_vec)), function(s) which(season_vec == s))
set.seed(42)
cv <- xgb.cv(params = params, data = dall, nrounds = 800, folds = folds,
             early_stopping_rounds = 40, verbose = 0)
elog <- as.data.frame(cv$evaluation_log)
best_n    <- which.min(elog$test_rmse_mean)
loso_rmse <- elog$test_rmse_mean[best_n]
message(sprintf("LOSO CV: best nrounds = %d, RMSE = %.3f points", best_n, loso_rmse))

# ---- final model on all seasons ---------------------------------------------
set.seed(42)
model <- xgb.train(params = params, data = dall, nrounds = best_n, verbose = 0)
xgb.save(model, file.path(model_dir, "xgb_model.json"))

imp <- xgb.importance(feature_names = feature_names, model = model)
write_csv(as.data.frame(imp)[, c("Feature", "Gain", "Cover", "Frequency")],
          file.path(model_dir, "feature_importance.csv"))

write_json(list(
  feature_names = feature_names,
  resid_sd      = round(loso_rmse, 3),
  loso_rmse     = round(loso_rmse, 3),
  nrounds       = best_n,
  seasons       = seasons,
  n_games       = nrow(games),
  trained_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
), file.path(model_dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE)

message("Model, meta and feature importance saved to ", model_dir, "/")
