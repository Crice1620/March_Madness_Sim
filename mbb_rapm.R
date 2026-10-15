#!/usr/bin/env Rscript
# mbb_rapm.R
# Calculates game-level Ridge RAPM with opponent adjustments using hoopR player box scores,
# and provides roster-aggregation helpers for post-transfer portal team projections.
# Usage: Rscript mbb_rapm.R [season]

suppressPackageStartupMessages({
  library(hoopR); library(dplyr); library(tidyr); library(readr); library(Matrix); library(glmnet)
})

args <- commandArgs(trailingOnly = TRUE)
cur_year <- as.integer(format(Sys.Date(), "%Y")); cur_month <- as.integer(format(Sys.Date(), "%m"))
season_arg <- if (length(args) >= 1) as.integer(args[1]) else ifelse(cur_month >= 11, cur_year + 1L, cur_year)

out_dir <- file.path("data/mbb", season_arg)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("=== Building Game-Level Ridge RAPM for Season ", season_arg, " ===")

# 1. Load Player Box Scores
pb <- tryCatch(load_mbb_player_box(seasons = season_arg), error = function(e) NULL)
if (is.null(pb) || nrow(pb) == 0) {
  message("No player box scores found for season ", season_arg, "; skipping RAPM calculation.")
  quit(status = 0)
}

# Clean and filter player box scores
pb_clean <- pb %>%
  transmute(
    game_id = as.character(game_id),
    team_id = as.character(team_id),
    player_id = as.character(athlete_id),
    player_name = athlete_display_name,
    minutes = suppressWarnings(as.numeric(minutes))
  ) %>%
  filter(!is.na(minutes), minutes > 0, !is.na(player_id))

# 2. Load Team Games to get Net Efficiency Target & Opponent mappings
games_path <- file.path(out_dir, "games.csv.gz")
if (!file.exists(games_path)) {
  stop("games.csv.gz not found in ", out_dir, ". Run mbb_backfill.R first!")
}
games <- read_csv(games_path, show_col_types = FALSE)

# Transform games into team-game rows (both home and away perspectives)
home_tg <- games %>% transmute(
  game_id, game_date, team_id = home_id, opp_id = away_id,
  net_margin = home_margin, game_poss, is_home = 1
)
away_tg <- games %>% transmute(
  game_id, game_date, team_id = away_id, opp_id = home_id,
  net_margin = -home_margin, game_poss, is_home = 0
)
team_games <- bind_rows(home_tg, away_tg)

# Join player minutes to team games and compute minute shares (out of 200 team minutes)
tg_players <- pb_clean %>%
  inner_join(team_games, by = c("game_id", "team_id")) %>%
  group_by(game_id, team_id) %>%
  mutate(min_share = minutes / sum(minutes, na.rm = TRUE)) %>%
  ungroup()

# 3. Construct Sparse Design Matrix for Ridge Regression
message("Constructing sparse design matrix for game-level RAPM...")
unique_players <- sort(unique(tg_players$player_id))
player_indices <- setNames(seq_along(unique_players), unique_players)

# Build sparse matrix components
tg_indexed <- tg_players %>%
  mutate(row_id = match(paste(game_id, team_id), paste(team_games$game_id, team_games$team_id)),
         col_id = player_indices[player_id]) %>%
  filter(!is.na(row_id), !is.na(col_id))

X <- sparseMatrix(
  i = tg_indexed$row_id,
  j = tg_indexed$col_id,
  x = tg_indexed$min_share,
  dims = c(nrow(team_games), length(unique_players))
)
colnames(X) <- unique_players
y <- team_games$net_margin

# 4. Fit Ridge Regression (Alpha = 0) with Cross-Validation
message("Fitting Ridge Regression model (glmnet)...")
cv_ridge <- cv.glmnet(X, y, alpha = 0, standardize = TRUE, nfolds = 5)
best_lambda <- cv_ridge$lambda.min

ridge_model <- glmnet(X, y, alpha = 0, lambda = best_lambda, standardize = TRUE)
coefs <- coef(ridge_model)[-1] # Remove intercept

player_ratings <- tibble(
  player_id = unique_players,
  rapm_rating = as.numeric(coefs)
) %>% left_join(
  tg_players %>% distinct(player_id, player_name, team_id),
  by = "player_id"
) %>% arrange(desc(rapm_rating))

# Save player ratings
write_csv(player_ratings, file.path(out_dir, "player_rapm.csv.gz"))
message("Saved opponent-adjusted player RAPM ratings to ", file.path(out_dir, "player_rapm.csv.gz"))

# 5. Transfer Portal Team Projection Function
# Stacks players into a new roster and projects team net efficiency per 100 possessions.
project_portal_team <- function(roster_df, player_rapm_table) {
  # roster_df requires: player_id, projected_minutes
  res <- roster_df %>%
    left_join(player_rapm_table, by = "player_id") %>%
    mutate(rapm_rating = coalesce(rapm_rating, 0),
           projected_minutes = coalesce(projected_minutes, 0))
  
  # Weight player RAPM by projected minute shares out of 200 total team minutes
  total_mins <- sum(res$projected_minutes, na.rm = TRUE)
  if (total_mins <= 0) return(0)
  
  projected_net <- sum(res$rapm_rating * (res$projected_minutes / total_mins))
  return(projected_net)
}

message("RAPM build and portal projection framework completed successfully.")
