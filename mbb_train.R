#!/usr/bin/env Rscript
# mbb_train.R
# Trains the margin model on COMPLETED seasons only, with leave-one-season-out validation.
# Usage: Rscript mbb_train.R [first_season] [last_season]
# Defaults: first = 2015, last = most recent completed season (season ends in April).
# Reads data/mbb/games_<season>.csv.gz, writes data/mbb_model/.

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(xgboost); library(jsonlite)
})
source("mbb_model_lib.R")

args <- commandArgs(trailingOnly = TRUE)
cur_year <- as.integer(format(Sys.Date(), "%Y")); cur_month <- as.integer(format(Sys.Date(), "%m"))
last_complete <- ifelse(cur_month <= 4, cur_year - 1L, cur_year)
first_season  <- if (length(args) >= 1) as.integer(args[1]) else 2015L
last_season   <- if (length(args) >= 2) as.integer(args[2]) else last_complete
exclude       <- c(2021L)   # COVID-distorted season
out_dir <- "data/mbb_model"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- load games -------------------------------------------------------------
files   <- list.files("data/mbb", pattern = "^games_\\d{4}\\.csv\\.gz$", full.names = TRUE)
seasons <- as.integer(gsub("\\D", "", basename(files)))
keep    <- seasons >= first_season & seasons <= last_season & !(seasons %in% exclude)
if (sum(keep) < 3) stop("Need at least 3 seasons of games files; found ", sum(keep))
message("Training seasons: ", paste(sort(seasons[keep]), collapse = ", "))

games <- bind_rows(lapply(files[keep], function(f)
  read_csv(f, col_types = cols(game_id = col_character(), home_id = col_character(),
                               away_id = col_character(), .default = col_guess()),
           show_col_types = FALSE)))
if (!"espn_only" %in% names(games)) games$espn_only <- 0
games <- games %>% filter(coalesce(espn_only, 0) == 0, !is.na(home_margin))

pull_side <- function(prefix)
  games %>% select(all_of(paste0(prefix, feat_cols))) %>% rename_with(~ sub(paste0("^", prefix), "", .x))
h <- pull_side("home_"); a <- pull_side("away_")

X <- matchup_features(h, a, games$neutral)
feature_names <- names(X)

# Neutral games have an arbitrary "home" side, so add the flipped copy (margin negated)
nz <- which(games$neutral == 1)
Xf <- matchup_features(a[nz, ], h[nz, ], 1)[, feature_names]
X_all      <- rbind(X, Xf)
y_all      <- c(games$home_margin, -games$home_margin[nz])
season_all <- c(games$season, games$season[nz])
is_aug     <- c(rep(0, nrow(X)), rep(1, length(nz)))
message(sprintf("%d games (%d neutral, flipped copies added) | %d features",
                nrow(games), length(nz), length(feature_names)))

# ---- params + choose number of rounds with season-grouped CV ----------------
params <- list(booster = "gbtree", objective = "reg:squarederror", eval_metric = "rmse",
               eta = 0.05, max_depth = 4, subsample = 0.8, colsample_bytree = 0.8,
               min_child_weight = 5, lambda = 1, seed = 42)
set.seed(42)
dall  <- xgb.DMatrix(as.matrix(X_all), label = y_all)
folds <- lapply(sort(unique(season_all)), function(s) which(season_all == s))

cv <- xgb.cv(params = params, data = dall, nrounds = 1000, folds = folds,
             early_stopping_rounds = 40, verbose = 0)
best_n <- tryCatch({
  n <- cv$best_iteration
  if (is.null(n) || length(n) == 0 || is.na(n) || n < 1) which.min(cv$evaluation_log$test_rmse_mean) else as.integer(n)
}, error = function(e) which.min(cv$evaluation_log$test_rmse_mean))
best_n <- max(1L, as.integer(best_n))
message("Rounds chosen by season-grouped CV: ", best_n)

# ---- honest leave-one-season-out predictions --------------------------------
oof <- rep(NA_real_, length(y_all)); base_pred <- rep(NA_real_, length(y_all))
dat_lm <- data.frame(y = y_all, d = X_all$adj_net_pre_diff, n = X_all$is_neutral)
for (i in seq_along(folds)) {
  te <- folds[[i]]; tr <- setdiff(seq_along(y_all), te)
  m <- xgb.train(params = params, data = xgb.DMatrix(as.matrix(X_all[tr, ]), label = y_all[tr]),
                 nrounds = best_n, verbose = 0)
  oof[te] <- predict(m, xgb.DMatrix(as.matrix(X_all[te, ])))
  base_pred[te] <- predict(lm(y ~ d + n, data = dat_lm[tr, ]), newdata = dat_lm[te, ])
}

ev  <- is_aug == 0
res <- y_all[ev] - oof[ev]
loso_rmse <- sqrt(mean(res^2)); loso_mae <- mean(abs(res))
resid_sd  <- sd(res)
early     <- X_all$min_gp[ev] < 8
resid_sd_early <- sd(res[early], na.rm = TRUE)
resid_sd_late  <- sd(res[!early], na.rm = TRUE)
base_rmse <- sqrt(mean((y_all[ev] - base_pred[ev])^2))
p_home <- pnorm(oof[ev] / resid_sd); won <- as.numeric(y_all[ev] > 0)
brier  <- mean((p_home - won)^2)
acc    <- mean((oof[ev] > 0) == (y_all[ev] > 0))

by_season <- tibble(season = season_all[ev], err = res) %>% group_by(season) %>%
  summarise(games = n(), rmse = sqrt(mean(err^2)), .groups = "drop")
print(as.data.frame(by_season))
message(sprintf(paste0("LOSO RMSE %.2f | MAE %.2f | resid SD %.2f (early-season %.2f, later %.2f)\n",
                       "Baseline (linear on adj-net diff + neutral) RMSE %.2f | SD of margins %.2f\n",
                       "Brier %.4f | straight-up accuracy %.1f%%"),
                loso_rmse, loso_mae, resid_sd, resid_sd_early, resid_sd_late,
                base_rmse, sd(y_all[ev]), brier, 100 * acc))

# ---- final model on all training seasons ------------------------------------
final <- xgb.train(params = params, data = dall, nrounds = best_n, verbose = 0)
xgb.save(final, file.path(out_dir, "xgb_model.json"))
write_csv(as.data.frame(xgb.importance(model = final)), file.path(out_dir, "feature_importance.csv"))
write_csv(by_season, file.path(out_dir, "loso_by_season.csv"))
write_json(list(
  trained_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  seasons = sort(unique(games$season)), excluded_seasons = exclude,
  n_games = nrow(games), nrounds = best_n, feature_names = feature_names, params = params[names(params) != "seed"],
  resid_sd = resid_sd, resid_sd_early = resid_sd_early, resid_sd_late = resid_sd_late,
  loso_rmse = loso_rmse, loso_mae = loso_mae, baseline_rmse = base_rmse, brier = brier, accuracy = acc
), file.path(out_dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE)
message("Saved model to ", out_dir)
