# function to create a full schedule for each team each season
create_full_game_records <- function(winning_games_df = read.csv("C:/Users/alexe/OneDrive/Documents/Sports Analysis/2025 CFB Data Modeling/Data Sources/unique cfb games.csv") |>
                                       rename(location = var_7,
                                              school = winner,
                                              opponent = loser)) {
  #' Create full game records including both winners and losers perspectives
  #' 
  #' @param winning_games_df Data frame with winning team records only
  #' @return Data frame with both perspectives of each game
  
  # Create inverse dataset (losers' perspective)
  inverse_games <- winning_games_df |>
    mutate(
      # Swap teams
      temp_school = school,
      school = opponent,
      opponent = temp_school,
      # Swap scores
      temp_pts = pts,
      pts = opp,
      opp = temp_pts,
      # Inverse location
      location = case_when(
        is.na(location) | location == '' ~ '@',  # Home becomes Away
        location == '@' ~ '',                     # Away becomes Home
        location == 'N' ~ 'N',                   # Neutral stays Neutral
        TRUE ~ location
      )
    ) |>
    select(-temp_school, -temp_pts)
  
  # Combine with original
  full_games <- bind_rows(winning_games_df, inverse_games) |>
    arrange(season, wk, date) |>
    select(season,wk,school,opponent)
  
  return(full_games)
}

# function to rank teams each week based on their ELO per season
create_weekly_rankings <- function(full_elo_df) {
  #' Create week-by-week ELO rankings for all teams across all seasons
  #' 
  #' Rankings show each team's ELO entering that week
  #' 
  #' @param full_elo_df The output from your ELO model with all games
  #' @return Data frame with season, week, team, elo, rank, rank_change
  
  library(tidyverse)
  
  full_elo_df <-full_elo_df |> filter(season == 2025)
  full_game_df <- create_full_game_records()
  
  # Step 1: For each team-week where they played, get their pre and post ELO
  games_played <- full_elo_df |>
    inner_join(conf_df,
               by = c('team_a' = 'school',
                      'season' = 'season')) |>
    mutate(
      game_result = paste0(result, " vs. ", team_b, " (", pts, "-", opp, ")"),
      elo_change_from_game = case_when(
        result == 'W' ~ elo_adj,
        result == 'L' ~ -elo_adj,
        result == 'T' ~ 0
      )
    ) |>
    select(season, wk, team = team_a, 
           elo_entering_week = elo_a,
           elo_change_from_game,
           game_result, 
           next_opp = team_b)
  
  # Step 2: Create complete season-week grid
  season_bounds <- full_elo_df |>
    group_by(season) |>
    summarise(first_wk = min(wk), last_wk = max(wk), .groups = 'drop')
  
  all_teams <- full_elo_df |>
    distinct(season, team = team_a) |>
    left_join(season_bounds, by = "season") |>
    rowwise() |>
    reframe(season = season, team = team, wk = first_wk:last_wk)
  
  # Step 3: Join and fill
  weekly_elo <- all_teams |>
    left_join(games_played, by = c("season", "wk", "team")) |>
    arrange(season, team, wk) |>
    group_by(season, team) |>
    mutate(
      elo_entering_week = ifelse(is.na(elo_entering_week),lag(elo_entering_week),elo_entering_week),
      pre_elo = ifelse(is.na(elo_change_from_game), elo_entering_week + 0, elo_entering_week - elo_change_from_game),
      ) |>
    fill(pre_elo, .direction = c('up')) |>
    mutate(post_elo = ifelse(is.na(elo_entering_week), pre_elo,elo_entering_week)) |>
    ungroup() |>
    # Calculate rankings
    group_by(season, wk) |>
    mutate(rank = min_rank(desc(post_elo))) |>
    ungroup() |>
    #filter(!is.na(game_result)) |>
    # Add next opponent and changes
    arrange(season, team, wk) |>
    group_by(season, team) |>
    mutate(
      next_week_opp = lead(next_opp),
      next_week_info = if_else(is.na(next_week_opp), "Bye Week", 
                               paste0("vs. ", next_week_opp)),
      last_result = if_else(is.na(game_result), "Bye Week", game_result),
      prev_rank = lag(rank),
      rank_change = prev_rank - rank
    ) |>
    ungroup() |>
    select(season, wk, team, 
           elo = post_elo, 
           rank, rank_change,
           elo_change = elo_change_from_game,
           last_result, next_week_info) |>
    arrange(season, wk, rank)
  
  return(weekly_elo)
}

# Function to get Top 25 for a specific week
get_top_25 <- function(weekly_rankings, season_num, week_num) {
  weekly_rankings |>
    filter(season == season_num, wk == week_num, rank <= 25) |>
    arrange(rank) |>
    select(season,wk, rank, team, elo, rank_change, elo_change, last_result, next_week_info)
}

# Function to find biggest movers in a week
get_biggest_movers <- function(weekly_rankings, season_num, week_num, n = 10) {
  week_data <- weekly_rankings |>
    filter(season == season_num, wk == week_num, !is.na(rank_change))
  
  biggest_up <- week_data |>
    arrange(desc(rank_change)) |>
    head(n) |>
    mutate(direction = "↑") |>
    select(season, direction, team, rank, rank_change, elo, elo_change, last_result)
  
  biggest_down <- week_data |>
    arrange(rank_change) |>
    head(n) |>
    mutate(direction = "↓") |>
    select(season,direction, team, rank, rank_change, elo, elo_change, last_result)
  
  list(movers_up = biggest_up, movers_down = biggest_down)
}

# Function to track a team's season trajectory
get_team_season_trajectory <- function(weekly_rankings, team_name, season_num) {
  weekly_rankings |>
    filter(team == team_name, season == season_num) |>
    select(season, wk, rank, elo, rank_change, elo_change, last_result, next_week_info) |>
    arrange(wk)
}

# function to pull WLT totals for active FBS teams
get_wlt_totals <- function(full_elo_df,conf_df){
  
  # Step 2: Create complete season-week grid
  season_bounds <- full_elo_df |>
    group_by(season) |>
    summarise(first_wk = min(wk), last_wk = max(wk), .groups = 'drop')
  
  all_teams <- full_elo_df |>
    distinct(season, team = team_a) |>
    #filter(team == 'Iowa') |>
    left_join(season_bounds, by = "season") |>
    rowwise() |>
    reframe(season = season, team = team, wk = first_wk:last_wk)
  
  team_weekly_record <- full_elo_df |>
    inner_join(conf_df,
               by = c('team_a' = 'school',
                      'season' = 'season')) |>
    select(-div) |>
    mutate(w = ifelse(pts > opp, 1, 0),
           l = ifelse(pts < opp, 1, 0),
           t = ifelse(pts == opp, 1, 0)) |>
    group_by(season,
             team_a) |>
    mutate(total_w = cumsum(w),
           total_l = cumsum(l),
           total_t = cumsum(t)) |>
    mutate(total_t = ifelse(total_t == 0, NA, total_t)) |>
    ungroup() |>
    unite(total_record, c('total_w','total_l','total_t'),sep='-',na.rm = T) |>
    select(season,
           wk,
           team = team_a,
           conf,
           total_record)
  
  all_teams |>
    left_join(team_weekly_record) |>
    group_by(team,
             season) |>
    fill(conf, .direction = c('down')) |>
    fill(conf, .direction = c('up')) |>
    fill(total_record, .direction = c('down')) |>
    mutate(total_record = ifelse(is.na(total_record),'0-0',total_record))
  
}

# Example usage
 weekly_rankings <- create_weekly_rankings(full_elo_df)
 weekly_season_wlt <- get_wlt_totals(full_elo_df,conf_df)
 
 get_team_season_trajectory(weekly_rankings, "Iowa State", 2000) |> as.data.frame()
 
 get_team_season_trajectory(weekly_rankings, team_name = 'Kansas State',2014)
 
 get_biggest_movers(weekly_rankings,2025,1)
 
 get_top_25(weekly_rankings,2022,10) |> as.data.frame() |> inner_join(weekly_season_wlt) |>
   select(season,
          wk,
          team,
          conf,
          total_record,
          rank,
          rank_change,
          elo,
          last_result,
          next_week_info)