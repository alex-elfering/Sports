# ==============================================================================
# CFB ELO Rating System - FUNCTIONS
# Save as: 20251003 CFB ELO Functions.R
# ==============================================================================

library(data.table)
library(tidyverse)

# DATA LOADING -----------------------------------------------------------------

load_data <- function(paths) {
  games <- read.csv(paths$games) |>
    distinct() |>
    rename(
      location = var_7,
      school = winner,
      opponent = loser
    ) |>
    select(-X) |>
    filter(!is.na(pts)) |>
    mutate(
      wk = as.numeric(wk),
      wins = as.integer(pts > opp),
      loses = as.integer(pts < opp),
      ties = as.integer(pts == opp)
    )
  
  conferences <- read.csv(paths$conferences) |>
    arrange(season) |>
    select(season, school, conf, div)
  
  list(games = games, conferences = conferences)
}

# HELPER FUNCTIONS -------------------------------------------------------------

get_location_adjustment <- function(location, rating, advantage) {
  case_when(
    location == '' ~ rating + advantage,   # Home
    location == '@' ~ rating - advantage,  # Away
    location == 'N' ~ rating               # Neutral
  )
}

flip_location <- function(location) {
  case_when(
    location == '' ~ '@',
    location == '@' ~ '',
    TRUE ~ 'N'
  )
}

# CORE ELO FUNCTIONS -----------------------------------------------------------

calculate_week_elo <- function(games_week, current_ratings, week, k_val, home_adv) {
  
  # Join ratings - make sure we're getting them
  result <- games_week |>
    left_join(current_ratings |> select(school, rating), by = c("school" = "school")) |>
    rename(school_elo = rating) |>
    left_join(current_ratings |> select(school, rating), by = c("opponent" = "school")) |>
    rename(opp_elo = rating)
  
  # Check for missing ratings - this should help debug
  if(any(is.na(result$school_elo)) | any(is.na(result$opp_elo))) {
    warning(sprintf("Missing ratings in week %d - some teams not in rating table", week))
  }
  
  result |>
    mutate(
      # Adjust for home field advantage
      school_elo_adj = get_location_adjustment(location, school_elo, home_adv),
      opp_elo_adj = get_location_adjustment(flip_location(location), opp_elo, home_adv),
      
      # Win probability
      elo_diff = abs(school_elo_adj - opp_elo_adj),
      m = (school_elo_adj - opp_elo_adj) / 400,
      p_team = 1 / (1 + 10^(-m)),
      
      # Margin of victory multiplier
      margin_abs = abs(pts - opp),
      mov_mult = log(margin_abs + 1) * (2.2 / (elo_diff * 0.001 + 2.2)),
      
      # ELO adjustment
      elo_adj = (k_val * mov_mult) * (1 - p_team),
      
      # Updated ratings
      team_elo_update = case_when(
        wins == 1 ~ school_elo + elo_adj,
        ties == 1 ~ school_elo,
        loses == 1 ~ school_elo - elo_adj
      ),
      opponent_elo_update = case_when(
        loses == 1 ~ opp_elo + elo_adj,
        ties == 1 ~ opp_elo,
        wins == 1 ~ opp_elo - elo_adj
      ),
      
      ps = round(elo_diff / 25, 1)
    ) |>
    select(
      season, wk, school, opponent, location, pts, opp, ps,
      elo_adj, p_team, p_opponent = p_team,
      school_elo = team_elo_update,
      opponent_elo = opponent_elo_update
    ) |>
    mutate(p_opponent = 1 - p_opponent)
}

update_ratings <- function(week_results, current_ratings) {
  # Get updated ratings from this week's games
  updated_schools <- week_results |> 
    select(school, rating = school_elo)
  
  updated_opponents <- week_results |> 
    select(school = opponent, rating = opponent_elo)
  
  # Combine all updates
  all_updates <- bind_rows(updated_schools, updated_opponents)
  
  # For teams that played, use their new rating; for teams that didn't, keep old rating
  teams_that_played <- unique(all_updates$school)
  
  current_ratings |>
    filter(!school %in% teams_that_played) |>
    bind_rows(all_updates) |>
    group_by(school) |>
    slice_tail(n = 1) |>  # In case a team played multiple games in one week
    ungroup() |>
    arrange(desc(rating))
}

regress_to_conference_mean <- function(ratings, conferences, params) {
  ratings |>
    left_join(conferences, by = "school") |>
    group_by(conf) |>
    mutate(mean_conf_elo = mean(rating, na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      overall_mean = mean(rating, na.rm = TRUE),
      regress_rating = case_when(
        is.na(conf) ~ params$initial_rating,
        conf == "Ind" ~ rating + params$season_regress_fraction * (overall_mean - rating),
        TRUE ~ rating + params$season_regress_fraction * (mean_conf_elo - rating)
      )
    ) |>
    select(school, rating = regress_rating)
}

# MAIN PROCESSING FUNCTION -----------------------------------------------------

process_season <- function(season_games, season_num, conferences, current_ratings, 
                           k_factor_fn, elo_params) {
  weeks <- sort(unique(season_games$wk))
  
  season_results <- list()
  ratings <- current_ratings
  
  for (week in weeks) {
    week_games <- filter(season_games, wk == week)
    
    week_results <- calculate_week_elo(
      week_games, 
      ratings, 
      week, 
      k_factor_fn(week),
      elo_params$home_advantage
    )
    
    ratings <- update_ratings(week_results, ratings)
    
    season_results[[length(season_results) + 1]] <- week_results
  }
  
  # End-of-season regression
  season_conf <- filter(conferences, season == season_num) |> select(school, conf)
  ratings <- regress_to_conference_mean(ratings, season_conf, elo_params)
  
  list(
    results = bind_rows(season_results),
    final_ratings = ratings
  )
}

# MAIN EXECUTION ---------------------------------------------------------------

run_elo_model <- function(paths, elo_params, k_factor_fn) {
  message("Loading data...")
  data <- load_data(paths)
  
  # Initialize
  all_teams <- unique(c(data$games$school, data$games$opponent))
  current_ratings <- tibble(school = all_teams, rating = elo_params$initial_rating)
  
  # DEBUG: Check initial setup
  message(sprintf("Total unique teams: %d", length(all_teams)))
  message(sprintf("Sample teams: %s", paste(head(all_teams, 5), collapse = ", ")))
  message(sprintf("Initial ratings table size: %d", nrow(current_ratings)))
  
  seasons <- sort(unique(data$games$season))
  all_results <- list()
  
  message("Processing seasons...")
  pb <- txtProgressBar(min = min(seasons), max = max(seasons), style = 3)
  
  for (season in seasons) {
    setTxtProgressBar(pb, season)
    
    season_games <- filter(data$games, season == .env$season)
    
    # DEBUG: First season only
    if(season == min(seasons)) {
      message(sprintf("\nFirst season (%d) - checking data:", season))
      message(sprintf("  Games this season: %d", nrow(season_games)))
      message(sprintf("  Sample game: %s vs %s", 
                      season_games$school[1], 
                      season_games$opponent[1]))
      message(sprintf("  School in ratings? %s", 
                      season_games$school[1] %in% current_ratings$school))
      message(sprintf("  Opponent in ratings? %s", 
                      season_games$opponent[1] %in% current_ratings$school))
    }
    
    season_output <- process_season(
      season_games, 
      season, 
      data$conferences, 
      current_ratings,
      k_factor_fn,
      elo_params
    )
    
    all_results[[length(all_results) + 1]] <- season_output$results
    current_ratings <- season_output$final_ratings
  }
  
  close(pb)
  message("\nProcessing complete!")
  
  bind_rows(all_results)
}

# FORMATTING OUTPUT ------------------------------------------------------------

format_full_elo_df <- function(elo_results) {
  # Original perspective
  df_a <- elo_results |>
    rename(
      team_a = school,
      team_b = opponent,
      elo_a = school_elo,
      elo_b = opponent_elo,
      p_a = p_team
    ) |>
    select(-p_opponent)
  
  # Opponent perspective
  df_b <- df_a |>
    select(season, wk, team_a = team_b, team_b = team_a, location, 
           pts = opp, opp = pts, ps, elo_adj, p_a, 
           elo_a = elo_b, elo_b = elo_a) |>
    mutate(
      p_a = 1 - p_a,
      location = flip_location(location)
    )
  
  bind_rows(df_a, df_b) |>
    mutate(result = case_when(
      pts > opp ~ 'W',
      pts < opp ~ 'L',
      TRUE ~ 'T'
    )) |>
    arrange(season, wk, team_a)
}