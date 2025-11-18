#### ELO MODEL GRID SEARCH
# Tests multiple parameter combinations and calculates Brier scores

library(here)
library(tidyverse)

# Pull in functions ----
source(here("20250603 CFB ELO Functions.R"))

# Define parameter grid ----
param_grid <- expand.grid(
  home_adv = c(25, 40, 50, 65, 80),
  regress_val = c(0.2, 0.3, 0.4, 0.5),
  k_early = c(30, 40, 50),
  k_mid = c(20, 30, 40),
  k_late = c(15, 20, 25)
) |>
  # Remove illogical combinations where k increases over time
  filter(k_early >= k_mid & k_mid >= k_late)

cat(sprintf("Testing %d parameter combinations\n\n", nrow(param_grid)))

# Helper function: Calculate Brier Score ----
calculate_brier_score <- function(predictions_df) {
  #' Calculate Brier Score for model predictions
  #' @param predictions_df Data frame with p_a (predicted prob) and actual result
  #' @return List with overall Brier score and by-season scores
  
  results <- predictions_df |>
    mutate(
      actual = case_when(
        result == 'W' ~ 1,
        result == 'L' ~ 0,
        result == 'T' ~ 0.5
      ),
      brier_sq_error = (p_a - actual)^2
    )
  
  overall_brier <- mean(results$brier_sq_error, na.rm = TRUE)
  
  season_brier <- results |>
    group_by(season) |>
    summarise(
      brier_score = mean(brier_sq_error, na.rm = TRUE),
      n_games = n(),
      .groups = 'drop'
    )
  
  return(list(
    overall = overall_brier,
    by_season = season_brier,
    predictions = results
  ))
}

# Modified k_dynamic to accept parameters ----
k_dynamic_param <- function(week, k_early, k_mid, k_late) {
  case_when(
    week <= 4 ~ k_early,
    week <= 8 ~ k_mid,
    TRUE ~ k_late
  )
}

# Modified process_season to use custom parameters ----
process_season_param <- function(season_data, starting_elo, season_num, conf_data, 
                                 home_adv, regress_val, k_early, k_mid, k_late) {
  
  beg_week <- min(season_data$wk)
  end_week <- max(season_data$wk)
  
  week_games <- list()
  current_elo <- starting_elo
  
  for(week_num in beg_week:end_week) {
    
    week_update <- elo_week_update(
      df = season_data,
      use_elo_df = current_elo,
      week_int = week_num,
      team_adv = home_adv,
      k_val = k_dynamic_param(week_num, k_early, k_mid, k_late)
    )
    
    if(nrow(week_update) == 0) {
      next
    }
    
    current_elo <- team_elo_scores(df = week_update, use_elo_df = current_elo)
    
    # Apply bounds
    current_elo <- current_elo |>
      mutate(rating = pmax(800, pmin(2400, rating)))
    
    week_games[[week_num]] <- week_update
    
    # Regress at season end
    if(week_num == end_week) {
      season_regress <- regress_ratings(
        conf_df = conf_data, 
        df = current_elo,
        regress_val = regress_val
      )
      
      current_elo <- season_regress |>
        select(school, rating = regress_rating)
    }
  }
  
  return(list(
    games = bind_rows(week_games),
    final_elo = current_elo
  ))
}

# Main grid search loop ----
all_results <- list()
season_vector <- c(1872:2024)
test_years <- c(2020:2024)  # Hold out for validation

# Data setup
all_schools <- winning_games$school
all_opponents <- winning_games$opponent
init_ratings <- tibble(
  school = unique(c(all_opponents, all_schools)),
  rating = 1500
)

# Progress bar setup
pb <- txtProgressBar(min = 0, max = nrow(param_grid), style = 3)

for(row_idx in 1:nrow(param_grid)) {
  
  params <- param_grid[row_idx, ]
  
  # Extract parameters
  home_adv <- params$home_adv
  regress_val <- params$regress_val
  k_early <- params$k_early
  k_mid <- params$k_mid
  k_late <- params$k_late
  
  # Run model with these parameters
  season_games <- list()
  current_ratings <- init_ratings
  
  for(season_year in season_vector) {
    
    use_season <- winning_games |>
      mutate(
        wk = as.numeric(wk),
        wins = ifelse(pts > opp, 1, 0),
        loses = ifelse(pts < opp, 1, 0),
        ties = ifelse(pts == opp, 1, 0)
      ) |>
      filter(season == season_year)
    
    season_conf <- conf_df |>
      filter(season == season_year) |>
      select(school, conf)
    
    season_results <- process_season_param(
      season_data = use_season,
      starting_elo = current_ratings,
      season_num = season_year,
      conf_data = season_conf,
      home_adv = home_adv,
      regress_val = regress_val,
      k_early = k_early,
      k_mid = k_mid,
      k_late = k_late
    )
    
    season_games[[season_year]] <- season_results$games
    current_ratings <- season_results$final_elo
  }
  
  # Combine all predictions
  all_predictions <- bind_rows(season_games) |>
    rename(
      team_a = school,
      team_b = opponent,
      p_a = p_team  # Keep the predicted probability
    ) |>
    mutate(result = case_when(
      pts > opp ~ 'W',
      pts < opp ~ 'L',
      pts == opp ~ 'T'
    ))
  
  # Calculate Brier score on FULL dataset
  brier_results_all <- calculate_brier_score(all_predictions)
  
  # Calculate Brier score on TEST set only (for validation)
  test_predictions <- all_predictions |> filter(season %in% test_years)
  brier_results_test <- calculate_brier_score(test_predictions)
  
  # Store results
  all_results[[row_idx]] <- list(
    params = params,
    brier_overall_all = brier_results_all$overall,
    brier_overall_test = brier_results_test$overall,
    brier_by_season_all = brier_results_all$by_season,
    brier_by_season_test = brier_results_test$by_season,
    n_predictions_all = nrow(all_predictions),
    n_predictions_test = nrow(test_predictions)
  )
  
  # Update progress
  setTxtProgressBar(pb, row_idx)
}

close(pb)

# Compile results into data frame ----
results_summary <- param_grid |>
  mutate(
    brier_all_data = map_dbl(all_results, ~.x$brier_overall_all),
    brier_test_set = map_dbl(all_results, ~.x$brier_overall_test),
    n_pred_all = map_dbl(all_results, ~.x$n_predictions_all),
    n_pred_test = map_dbl(all_results, ~.x$n_predictions_test)
  ) |>
  arrange(brier_test_set)  # Sort by test set performance!

# Extract season-level details for top models ----
top_n_models <- 10
season_details <- list()

for(i in 1:top_n_models) {
  season_details[[i]] <- all_results[[i]]$brier_by_season_test |>
    mutate(
      model_rank = i,
      home_adv = results_summary$home_adv[i],
      regress_val = results_summary$regress_val[i],
      k_early = results_summary$k_early[i],
      k_mid = results_summary$k_mid[i],
      k_late = results_summary$k_late[i]
    )
}

season_brier_detail <- bind_rows(season_details)

# Export results ----
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_dir <- "C:/Users/alexe/OneDrive/Documents/Sports Analysis/CFB ELO Model/"

write.csv(results_summary, 
          paste0(output_dir, "grid_search_results_", timestamp, ".csv"), 
          row.names = FALSE)

write.csv(season_brier_detail, 
          paste0(output_dir, "season_brier_details_", timestamp, ".csv"), 
          row.names = FALSE)

# Display top 10 results ----
cat("\n\n=== TOP 10 PARAMETER COMBINATIONS (by test set performance) ===\n\n")
print(results_summary |> 
        head(10) |> 
        select(home_adv, regress_val, k_early, k_mid, k_late, 
               brier_test_set, brier_all_data))

cat(sprintf("\n✓ Grid search complete! Results saved with timestamp: %s\n", timestamp))
cat(sprintf("  - Training data: 1872-2019 (%d games)\n", results_summary$n_pred_all[1] - results_summary$n_pred_test[1]))
cat(sprintf("  - Test data: 2020-2024 (%d games)\n", results_summary$n_pred_test[1]))

# Optional: Visualization of results ----
library(ggplot2)

# Brier score by home advantage
ggplot(results_summary, aes(x = home_adv, y = brier_score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Brier Score by Home Field Advantage",
       x = "Home Advantage",
       y = "Brier Score (lower is better)") +
  theme_minimal()

ggsave(paste0(output_dir, "brier_by_home_adv_", timestamp, ".png"), 
       width = 8, height = 6)

# Brier score by regression value
ggplot(results_summary, aes(x = regress_val, y = brier_score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Brier Score by Regression Value",
       x = "Regression Value",
       y = "Brier Score (lower is better)") +
  theme_minimal()

ggsave(paste0(output_dir, "brier_by_regress_", timestamp, ".png"), 
       width = 8, height = 6)

cat("\n✓ Visualizations saved!\n")