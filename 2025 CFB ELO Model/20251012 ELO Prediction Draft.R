################################################################################
# CFB ELO Season Simulator - Master Script
# Progressive season simulation using ELO ratings
################################################################################

# Load Libraries ----
library(tidyverse)
library(data.table)
library(here)

# Source Required Scripts ----
source(here("20250603 CFB ELO Functions.R"))  # ELO calculation functions
source(here("20250603 CFB ELO Model.R"))      # Main ELO model (creates full_elo_df)
source(here("20250603 Weekly Rankings.R"))    # Weekly rankings function

# Load/Create Core Data ----
# These should be created by the sourced scripts above:
# - full_elo_df: All games with ELO ratings
# - weekly_rankings: Week-by-week ELO rankings
# - conf_df: Conference membership data

# If not created by sourced scripts, load manually:
if(!exists("full_elo_df")) {
  full_elo_df <- read.csv("C:/Users/alexe/OneDrive/Documents/Sports Analysis/CFB ELO Model/full_elo_df.csv")
}

if(!exists("weekly_rankings")) {
  weekly_rankings <- create_weekly_rankings(full_elo_df)
}

if(!exists("conf_df")) {
  conf_df <- read.csv("C:/Users/alexe/OneDrive/Documents/Sports Analysis/2025 CFB Data Modeling/Data Sources/school conferences.csv") |>
    select(season, school, conf, div)
}

# Get future schedule for incomplete seasons
schedule_2025 <- create_full_game_records() |>
  filter(season == 2025)

################################################################################
# HELPER FUNCTIONS
################################################################################

# Deduplicate games ----
deduplicate_games <- function(games_df) {
  #' Remove duplicate game entries (keeps only one side of each matchup)
  games_df |>
    rowwise() |>
    mutate(
      game_key = paste(season, wk, 
                       paste(sort(c(team_a, team_b)), collapse = "_"),
                       sep = "_")
    ) |>
    ungroup() |>
    distinct(game_key, .keep_all = TRUE) |>
    select(-game_key)
}

# Calculate records to date ----
calculate_records_to_date <- function(games, teams) {
  #' Calculate win-loss records from completed games
  
  team_a_records <- games |>
    group_by(team_a) |>
    summarise(
      wins = sum(result == "W"),
      losses = sum(result == "L"),
      ties = sum(result == "T"),
      .groups = "drop"
    ) |>
    rename(team = team_a)
  
  team_b_records <- games |>
    group_by(team_b) |>
    summarise(
      wins = sum(result == "L"),
      losses = sum(result == "W"),
      ties = sum(result == "T"),
      .groups = "drop"
    ) |>
    rename(team = team_b)
  
  all_teams <- tibble(team = teams)
  
  bind_rows(team_a_records, team_b_records) |>
    group_by(team) |>
    summarise(
      wins = sum(wins),
      losses = sum(losses),
      ties = sum(ties),
      .groups = "drop"
    ) |>
    right_join(all_teams, by = "team") |>
    mutate(
      wins = replace_na(wins, 0),
      losses = replace_na(losses, 0),
      ties = replace_na(ties, 0)
    )
}

# Run season simulations with win-out/lose-out tracking ----
run_season_simulations <- function(current_records, remaining_games, n_sims) {
  #' Simulate remaining games and aggregate results
  
  n_games <- nrow(remaining_games)
  sim_matrix <- matrix(NA, nrow = n_sims, ncol = n_games)
  
  for(i in 1:n_sims) {
    sim_matrix[i, ] <- rbinom(n_games, 1, remaining_games$win_prob_a)
  }
  
  teams <- unique(c(remaining_games$team_a, remaining_games$team_b))
  
  final_records <- map_dfr(teams, ~{
    team_name <- .x
    
    current <- current_records |> filter(team == team_name)
    current_wins <- if(nrow(current) > 0) current$wins else 0
    current_losses <- if(nrow(current) > 0) current$losses else 0
    
    games_as_a <- which(remaining_games$team_a == team_name)
    wins_as_a <- if(length(games_as_a) > 0) {
      rowSums(sim_matrix[, games_as_a, drop = FALSE])
    } else {
      rep(0, n_sims)
    }
    
    games_as_b <- which(remaining_games$team_b == team_name)
    wins_as_b <- if(length(games_as_b) > 0) {
      rowSums(1 - sim_matrix[, games_as_b, drop = FALSE])
    } else {
      rep(0, n_sims)
    }
    
    total_wins <- current_wins + wins_as_a + wins_as_b
    remaining_wins <- wins_as_a + wins_as_b
    n_remaining <- length(games_as_a) + length(games_as_b)
    total_games <- current_wins + current_losses + n_remaining
    
    # NEW: Calculate win-out and lose-out probabilities
    prob_win_out <- if(n_remaining > 0) {
      sum(remaining_wins == n_remaining) / n_sims
    } else {
      NA_real_
    }
    
    prob_lose_out <- if(n_remaining > 0) {
      sum(remaining_wins == 0) / n_sims
    } else {
      NA_real_
    }
    
    tibble(
      team = team_name,
      current_wins = current_wins,
      current_losses = current_losses,
      games_remaining = n_remaining,
      mean_final_wins = mean(total_wins),
      median_final_wins = median(total_wins),
      sd_wins = sd(total_wins),
      min_wins = min(total_wins),
      max_wins = max(total_wins),
      prob_6plus = sum(total_wins >= 6) / n_sims,
      prob_8plus = sum(total_wins >= 8) / n_sims,
      prob_10plus = sum(total_wins >= 10) / n_sims,
      prob_undefeated = sum(total_wins == total_games) / n_sims,
      prob_win_out = prob_win_out,      # NEW
      prob_lose_out = prob_lose_out     # NEW
    )
  }) |>
    arrange(desc(mean_final_wins))
  
  return(list(
    final_records = final_records,
    remaining_games = remaining_games |> 
      select(wk, team_a, team_b, location, win_prob_a),
    simulation_matrix = sim_matrix
  ))
}

# Also update the early return case in simulate_season_progressive:
# Find this section and add the two new columns:

################################################################################
# MAIN SIMULATION FUNCTION
################################################################################

simulate_season_progressive <- function(weekly_rankings, full_elo_df, conf_df,
                                        season_num, as_of_week, 
                                        future_schedule = NULL,
                                        conference_name = NULL,
                                        conference_games_only = FALSE,
                                        n_sims = 10000) {
  #' Simulate remaining games from a specific week using ELO at that point
  
  # Get teams to simulate
  if(!is.null(conference_name)) {
    teams_to_sim <- conf_df |>
      filter(season == season_num, conf == conference_name) |>
      pull(school)
  } else {
    if(!is.null(future_schedule) && season_num > max(full_elo_df$season, na.rm = TRUE)) {
      teams_to_sim <- unique(future_schedule$team[future_schedule$season == season_num])
    } else {
      teams_to_sim <- unique(full_elo_df$team_a[full_elo_df$season == season_num])
    }
  }
  
  # Get completed games (ALL games for these teams)
  completed_games <- full_elo_df |>
    filter(season == season_num, 
           wk <= as_of_week,
           team_a %in% teams_to_sim) |>
    select(season, wk, team_a, team_b, result) |>
    deduplicate_games()
  
  # Calculate current records
  current_records <- calculate_records_to_date(completed_games, teams_to_sim)
  
  # Get remaining games
  if(!is.null(future_schedule)) {
    remaining_games <- future_schedule |>
      filter(season == season_num,
             wk > as_of_week,
             team %in% teams_to_sim,
             opponent != "Bye Week") |>
      filter(if(conference_games_only) opponent %in% teams_to_sim else TRUE) |>
      select(season, wk, team_a = team, team_b = opponent, location) |>
      deduplicate_games()
  } else {
    remaining_games <- full_elo_df |>
      filter(season == season_num,
             wk > as_of_week,
             team_a %in% teams_to_sim) |>
      filter(if(conference_games_only) team_b %in% teams_to_sim else TRUE) |>
      select(season, wk, team_a, team_b, location) |>
      deduplicate_games()
  }
  
  # NOW check if empty - AFTER remaining_games exists
  if(nrow(remaining_games) == 0) {
    cat("No remaining games to simulate\n")
    return(list(
      final_records = current_records |>
        mutate(
          games_remaining = 0,
          mean_final_wins = wins,
          median_final_wins = wins,
          sd_wins = 0,
          min_wins = wins,
          max_wins = wins,
          prob_6plus = as.numeric(wins >= 6),
          prob_8plus = as.numeric(wins >= 8),
          prob_10plus = as.numeric(wins >= 10),
          prob_undefeated = as.numeric(wins + losses == 0 | (wins > 0 & losses == 0)),
          prob_win_out = NA_real_,
          prob_lose_out = NA_real_
        ) |>
        rename(current_wins = wins, current_losses = losses) |>
        arrange(desc(mean_final_wins)),
      remaining_games = remaining_games,
      simulation_matrix = NULL
    ))
  }
  
  # Get ELO ratings as of specified week
  elo_as_of <- weekly_rankings |>
    filter(season == season_num, wk == as_of_week) |>
    select(team, elo)
  
  # Calculate win probabilities
  remaining_with_probs <- remaining_games |>
    left_join(elo_as_of, by = c("team_a" = "team")) |>
    rename(elo_a = elo) |>
    left_join(elo_as_of, by = c("team_b" = "team")) |>
    rename(elo_b = elo) |>
    filter(!is.na(elo_a), !is.na(elo_b)) |>
    mutate(
      elo_a_adj = elo_a + case_when(
        location == '' | is.na(location) ~ 50,
        location == '@' ~ -50,
        location == 'N' ~ 0,
        TRUE ~ 0
      ),
      elo_b_adj = elo_b + case_when(
        location == '@' ~ 50,
        location == '' | is.na(location) ~ -50,
        location == 'N' ~ 0,
        TRUE ~ 0
      ),
      win_prob_a = 1 / (1 + 10^(-(elo_a_adj - elo_b_adj) / 400))
    )
  
  cat(sprintf("Simulating %s season %d from week %d\n", 
              ifelse(is.null(conference_name), "full", conference_name),
              season_num, as_of_week + 1))
  cat(sprintf("Teams: %d, Completed: %d, Remaining: %d\n",
              length(teams_to_sim), nrow(completed_games), nrow(remaining_with_probs)))
  
  # Run simulations
  sim_results <- run_season_simulations(
    current_records = current_records,
    remaining_games = remaining_with_probs,
    n_sims = n_sims
  )
  
  return(sim_results)
}

# Wrapper for timeline simulations ----
simulate_season_timeline <- function(weekly_rankings, full_elo_df, conf_df,
                                     season_num, weeks_to_simulate,
                                     future_schedule = NULL,
                                     conference_name = NULL,
                                     conference_games_only = FALSE,
                                     n_sims = 1000) {
  #' Run simulations from multiple points in the season
  
  results_by_week <- map(weeks_to_simulate, ~{
    cat(sprintf("\n=== Simulating from Week %d ===\n", .x))
    
    sim <- simulate_season_progressive(
      weekly_rankings = weekly_rankings,
      full_elo_df = full_elo_df,
      conf_df = conf_df,
      season_num = season_num,
      as_of_week = .x,
      future_schedule = future_schedule,
      conference_name = conference_name,
      conference_games_only = conference_games_only,
      n_sims = n_sims
    )
    
    list(
      as_of_week = .x,
      projections = sim$final_records
    )
  })
  
  names(results_by_week) <- paste0("week_", weeks_to_simulate)
  return(results_by_week)
}

################################################################################
# USAGE EXAMPLES
################################################################################

cat("\n=== CFB Season Simulator Ready ===\n\n")

# Example 1: Simulate 2025 full season from current week ----
sim_2025 <- simulate_season_progressive(
  weekly_rankings = weekly_rankings,
  full_elo_df = full_elo_df,
  conf_df = conf_df,
  season_num = 2025,
  as_of_week = 7,
  future_schedule = schedule_2025,
  conference_games_only = FALSE,
  n_sims = 10000
)

cat("\n2025 Season Projections (Top 10):\n")
print(sim_2025$final_records |>
        mutate(total_games = current_wins + current_losses + games_remaining) |>
        select(team, current_wins, current_losses, games_remaining, 
               mean_final_wins, prob_10plus) |>
        head(10))

# Example 2: Big Ten championship race ----
big_ten_2025 <- simulate_season_progressive(
  weekly_rankings = weekly_rankings,
  full_elo_df = full_elo_df,
  conf_df = conf_df,
  season_num = 2025,
  as_of_week = 7,
  future_schedule = schedule_2025,
  conference_name = "Big Ten",
  conference_games_only = TRUE,
  n_sims = 10000
)

cat("\nBig Ten Championship Race:\n")
print(big_ten_2025$final_records |> head(10))

# Example 3: Historical season (1999) ----
sim_1999 <- simulate_season_progressive(
  weekly_rankings = weekly_rankings,
  full_elo_df = full_elo_df,
  conf_df = conf_df,
  season_num = 1999,
  as_of_week = 8,
  future_schedule = NULL,
  n_sims = 10000
)

cat("\n1999 Season Projections (from Week 8):\n")
print(sim_1999$final_records |> head(10))

# Example 4: Track projections over time ----
timeline_2025 <- simulate_season_timeline(
  weekly_rankings = weekly_rankings,
  full_elo_df = full_elo_df,
  conf_df = conf_df,
  season_num = 2025,
  weeks_to_simulate = 1:8,
  future_schedule = schedule_2025,
  n_sims = 10000
)

# How did Ohio State's projection change?
ohio_state_evolution <- map_dfr(c(2, 4, 6, 7), ~{
  timeline_2025[[paste0("week_", .x)]]$projections |>
    filter(team == "Ohio State") |>
    mutate(projection_week = .x)
})

cat("\nOhio State Projection Evolution:\n")
print(ohio_state_evolution |> 
        select(projection_week, current_wins, mean_final_wins, prob_10plus))

cat("\n=== Simulation Complete ===\n")

# Example 1: Full season projections with win-out/lose-out
sim_2025 <- simulate_season_progressive(
  weekly_rankings = weekly_rankings,
  full_elo_df = full_elo_df,
  conf_df = conf_df,
  season_num = 2025,
  as_of_week = 8,
  future_schedule = schedule_2025,
  conference_games_only = FALSE,
  n_sims = 10000
)

# View teams most likely to win out
cat("\nTeams Most Likely to Win Out:\n")
sim_2025$final_records |>
  filter(games_remaining > 0) |>
  arrange(desc(prob_win_out)) |>
  select(team, current_wins, current_losses, games_remaining, 
         mean_final_wins, prob_win_out, prob_lose_out) |>
  head(15)

# View teams in danger of losing out
cat("\nTeams Most Likely to Lose Out:\n")
sim_2025$final_records |>
  filter(games_remaining > 0) |>
  arrange(desc(prob_lose_out)) |>
  select(team, current_wins, current_losses, games_remaining, 
         mean_final_wins, prob_win_out, prob_lose_out) |>
  head(15)

# Find teams with interesting scenarios
cat("\nTeams with Volatile Finishes:\n")
sim_2025$final_records |>
  filter(games_remaining > 0) |>
  mutate(volatility = prob_win_out + prob_lose_out) |>
  arrange(desc(volatility)) |>
  select(team, current_wins, current_losses, games_remaining,
         prob_win_out, prob_lose_out, volatility) |>
  head(10)

# Bowl eligibility scenarios
cat("\nBowl Eligibility Scenarios:\n")
sim_2025$final_records |>
  filter(current_wins < 6, games_remaining > 0) |>
  mutate(
    needs_to_win = 6 - current_wins,
    can_reach_bowl = games_remaining >= needs_to_win
  ) |>
  select(team, current_wins, games_remaining, needs_to_win, 
         prob_6plus, prob_win_out, prob_lose_out) |>
  arrange(desc(prob_6plus))

# Visualization: Win-out vs Lose-out scatter plot
library(ggplot2)

sim_2025$final_records |>
  filter(games_remaining > 0) |>
  ggplot(aes(x = prob_lose_out, y = prob_win_out)) +
  geom_point(aes(size = games_remaining, color = current_wins), alpha = 0.6) +
  geom_text(aes(label = team), hjust = -0.1, size = 3, 
            data = . %>% filter(prob_win_out > 0.3 | prob_lose_out > 0.3)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_gradient(low = "#d7191c", high = "#2c7bb6") +
  labs(
    title = "2025 CFB Season - Win Out vs Lose Out Probabilities",
    subtitle = "From Week 7 projections",
    x = "Probability of Losing Out (0% rest of season)",
    y = "Probability of Winning Out (100% rest of season)",
    size = "Games Remaining",
    color = "Current Wins"
  ) +
  theme_minimal()

# Conference-specific: Big Ten teams most likely to win out
big_ten_2025 <- simulate_season_progressive(
  weekly_rankings = weekly_rankings,
  full_elo_df = full_elo_df,
  conf_df = conf_df,
  season_num = 2025,
  as_of_week = 8,
  future_schedule = schedule_2025,
  conference_name = "Big Ten",
  conference_games_only = TRUE,
  n_sims = 10000
)

cat("\nBig Ten: Conference Win-Out Probabilities:\n")
big_ten_2025$final_records |>
  filter(games_remaining > 0) |>
  arrange(desc(prob_win_out)) |>
  select(team, current_wins, current_losses, games_remaining, prob_win_out)

# Calculate theoretical win-out probability (product of individual game probs)
theoretical_win_out <- function(sim_results, team_name) {
  #' Calculate theoretical probability of winning out based on game-by-game odds
  
  team_games <- sim_results$remaining_games |>
    filter(team_a == team_name | team_b == team_name)
  
  if(nrow(team_games) == 0) {
    return(NA)
  }
  
  # Get win probability for each game
  win_probs <- team_games |>
    mutate(
      team_win_prob = if_else(team_a == team_name, win_prob_a, 1 - win_prob_a)
    ) |>
    pull(team_win_prob)
  
  # Probability of winning all games (independence assumption)
  prod(win_probs)
}

# Compare simulated vs theoretical
comparison <- sim_2025$final_records |>
  filter(games_remaining > 0, !is.na(prob_win_out)) |>
  rowwise() |>
  mutate(
    theoretical = theoretical_win_out(sim_2025, team)
  ) |>
  ungroup() |>
  select(team, games_remaining, prob_win_out, theoretical) |>
  mutate(difference = prob_win_out - theoretical) |>
  arrange(desc(prob_win_out))

cat("\nSimulated vs Theoretical Win-Out Probabilities:\n")
print(comparison |> head(15))