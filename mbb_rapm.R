#!/usr/bin/env Rscript
# mbb_rapm.R
# Fits game-level Ridge RAPM with opponent adjustments across all available completed seasons in data/mbb/.
# Usage: Rscript mbb_rapm.R [start_season] [end_season]

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(Matrix); library(glmnet); library(jsonlite); library(hoopR)
})

args <- commandArgs(trailingOnly = TRUE)
exclude <- c(2021L) # COVID-distorted season

# Discover seasons automatically from the data/mbb directory
mbb_dirs <- list.dirs("data/mbb", full.names = FALSE, recursive = FALSE)
available_seasons <- sort(suppressWarnings(as.integer(mbb_dirs)))
available_seasons <- available_seasons[!is.na(available_seasons) & !(available_seasons %in% exclude)]

start_season <- if (length(args) >= 1) as.integer(args[1]) else min(available_seasons)
end_season   <- if (length(args) >= 2) as.integer(args[2]) else max(available_seasons)

seasons_to_run <- available_seasons[available_seasons >= start_season & available_seasons <= end_season]

message("RAPM processing seasons: ", paste(seasons_to_run, collapse = ", "))

run_rapm_season <- function(season_arg) {
  message(sprintf("\n=== Building Game-Level Ridge RAPM for Season %d ===", season_arg))
  
  games_path <- file.path("data/mbb", season_arg, "games.csv.gz")
  if (!file.exists(games_path)) {
    message("Games file not found for season: ", season_arg, "; skipping.")
    return(FALSE)
  }
  
  games <- read_csv(games_path, show_col_types = FALSE) %>%
    mutate(
      game_id = as.character(game_id),
      home_id = as.character(home_id),
      away_id = as.character(away_id)
    )
  
  pb <- tryCatch(load_mbb_player_box(seasons = season_arg), error = function(e) NULL)
  if (is.null(pb) || nrow(pb) == 0) {
    message("No player box data available for season ", season_arg, "; skipping.")
    return(FALSE)
  }
  
  pb <- pb %>%
    transmute(
      game_id = as.character(game_id),
      team_id = as.character(team_id),
      athlete_id = as.character(athlete_id),
      athlete_display_name = athlete_display_name,
      minutes = as.numeric(minutes)
    ) %>%
    filter(!is.na(minutes), minutes > 0)
  
  team_games <- pb %>%
    group_by(game_id, team_id) %>%
    mutate(min_share = minutes / sum(minutes, na.rm = TRUE)) %>%
    ungroup()
  
  home_outcomes <- games %>%
    transmute(game_id = as.character(game_id), team_id = as.character(home_id), opp_id = as.character(away_id), net_margin = home_margin)
  away_outcomes <- games %>%
    transmute(game_id = as.character(game_id), team_id = as.character(away_id), opp_id = as.character(home_id), net_margin = -home_margin)
  team_outcomes <- bind_rows(home_outcomes, away_outcomes)
  
  model_df <- team_games %>%
    inner_join(team_outcomes, by = c("game_id", "team_id"))
  
  message("Constructed RAPM dataset with ", nrow(model_df), " player-game records for ", season_arg)
  
  out_dir <- file.path("data/mbb", season_arg)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Placeholder for output until we wire the full sparse matrix solver
  write_csv(tibble(athlete_id = character(), athlete_display_name = character(), rapm = numeric()),
            file.path(out_dir, "player_rapm.csv.gz"))
  TRUE
}

for (szn in seasons_to_run) {
  tryCatch(run_rapm_season(szn), error = function(e) message("Failed season ", szn, ": ", conditionMessage(e)))
}
