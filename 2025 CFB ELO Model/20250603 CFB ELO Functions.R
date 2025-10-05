# ELO functions

library(data.table)
library(tidyverse)
library(ggplot2)
library(rvest)
library(stringi)
library(glue)
library(here)

# Configuration Constants ----
INITIAL_RATING <- 1500
HOME_ADVANTAGE <- 25
REGRESS_AMOUNT <- 0.4
K_VALUE <- 50

# data sources  ----
winning_games <- read.csv("C:/Users/alexe/OneDrive/Documents/Sports Analysis/2025 CFB Data Modeling/Data Sources/unique cfb games.csv") |>
  distinct() |>
  rename(location = var_7,
         school = winner,
         opponent = loser) |>
  select(-X) |>
  filter(!is.na(pts))

conf_df <- read.csv("C:/Users/alexe/OneDrive/Documents/Sports Analysis/2025 CFB Data Modeling/Data Sources/school conferences.csv") |> arrange(season) |> select(season, school, conf, div)

# helper functions ----
change_school_names <- function(df, column_name, new_name){
  
  column_name = enquo(column_name)
  
  df_new <- df |>
    mutate(new_name = case_when(!!column_name == 'UTEP' ~ 'Texas-El Paso',
                                !!column_name == 'UAB' ~ 'Alabama-Birmingham',
                                !!column_name == 'BYU' ~ "Brigham Young",
                                !!column_name == 'UCF' ~ "Central Florida",
                                !!column_name == "LSU" ~ "Louisiana State",
                                !!column_name == "Ole Miss" ~ "Mississippi",
                                !!column_name == "Pitt" ~ "Pittsburgh",
                                !!column_name == "USC" ~ 'Southern California',
                                !!column_name == 'SMU' ~ "Southern Methodist",
                                !!column_name == 'UTSA' ~ 'Texas-San Antonio',
                                TRUE ~ !!column_name))
  
  return(df_new)
  
}

get_location_adjustment <- function(location, team_adv) {
  #' Calculate ELO adjustment based on game location (vectorized)
  #' 
  #' @param location Game location: '' = home, '@' = away, 'N' = neutral (can be vector)
  #' @param team_adv Home field advantage value
  #' @return Numeric adjustment to add to team's ELO
  
  case_when(
    location == '' | is.na(location) ~ team_adv,     # Home advantage
    location == '@' ~ -team_adv,                      # Away disadvantage
    location == 'N' ~ 0,                              # Neutral site
    TRUE ~ 0                                          # Default
  )
}

# core ELO functions ----
elo_week_update <- function(df, use_elo_df, week_int, team_adv = HOME_ADVANTAGE, k_val = K_VALUE) {
  #' Update ELO Ratings for One Week
  #'
  #' @param df Game-level data (must include 'wk', 'school', 'opponent', 'pts', 'opp', 'wins', 'loses', 'ties', 'location')
  #' @param use_elo_df Data frame of current ELO ratings (must include 'school' and 'rating')
  #' @param week_int Integer for the week number to calculate
  #' @param team_adv Home field advantage (default = HOME_ADVANTAGE constant)
  #' @param k_val ELO k-factor (default = 30)
  #'
  #' @return A data frame with updated team and opponent ELOs and match results
  
  required_cols <- c("wk", "school", "opponent", "pts", "opp", "wins", "loses", "ties", "location")
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns in df")
  }
  
  # Safety check for empty weeks
  week_data <- df |> dplyr::filter(wk == week_int)
  if (nrow(week_data) == 0) {
    message(sprintf("No games found for week %d", week_int))
    return(data.frame())
  }
  
  week_data |>
    dplyr::left_join(use_elo_df, by = c("school" = "school")) |>
    dplyr::rename(school_elo = rating) |>
    dplyr::left_join(use_elo_df, by = c("opponent" = "school")) |>
    dplyr::rename(opp_elo = rating) |>
    dplyr::mutate(
      # Apply location adjustments using helper function
      school_elo_adj = school_elo + get_location_adjustment(location, team_adv),
      opp_elo_adj = opp_elo + get_location_adjustment(
        case_when(
          location == '' ~ '@',
          location == '@' ~ '',
          TRUE ~ 'N'
        ), 
        team_adv
      ),
      
      # Calculate win probabilities
      m = (school_elo_adj - opp_elo_adj) / 400,
      elo_diff = abs(school_elo_adj - opp_elo_adj),
      ps = round(elo_diff / 25, 1),
      p_opponent = 1 / (1 + 10^m),
      p_team = 1 - p_opponent,
      
      # Margin of victory multiplier with diminishing returns for blowouts
      # Scaled by pre-game ELO difference to reduce impact of expected blowouts
      margin_abs = abs(pts - opp),
      ln_margin = log(margin_abs + 1),                    # Logarithmic diminishing returns
      elo_scaling = 2.2 / (elo_diff * 0.001 + 2.2),      # Reduces impact when big favorite wins big
      mov_mult = ln_margin * elo_scaling,
      
      # Calculate ELO change
      elo_adj = (k_val * mov_mult) * (1 - p_team),
      
      # Update ratings based on result
      team_elo_update = dplyr::case_when(
        wins == 1 ~ school_elo + elo_adj,
        ties == 1 ~ school_elo,
        loses == 1 ~ school_elo - elo_adj
      ),
      opponent_elo_update = dplyr::case_when(
        loses == 1 ~ opp_elo + elo_adj,
        ties == 1 ~ opp_elo,
        wins == 1 ~ opp_elo - elo_adj
      )
    ) |>
    dplyr::select(
      season, wk, school, opponent, location, pts, opp, ps,
      elo_adj, p_team, p_opponent,
      school_elo = team_elo_update,
      opponent_elo = opponent_elo_update
    )
}

team_elo_scores <- function(df, use_elo_df) {
  #' Combine Current Ratings with New ELO Scores
  #'
  #' @param df Weekly update from `elo_week_update`
  #' @param use_elo_df Existing ELO ratings
  #'
  #' @return Data frame of latest ratings
  
  school_elo <- df |> dplyr::select(school, rating = school_elo)
  opponent_elo <- df |> dplyr::select(school = opponent, rating = opponent_elo)
  
  dplyr::bind_rows(use_elo_df, school_elo, opponent_elo) |>
    dplyr::group_by(school) |>
    dplyr::mutate(index = dplyr::row_number()) |>
    dplyr::filter(index == max(index)) |>
    dplyr::ungroup() |>
    dplyr::arrange(desc(rating))
}

regress_ratings <- function(conf_df, df, regress_val = REGRESS_AMOUNT) {
  #' Regress Ratings Toward Conference Mean
  #' 
  #' Prevents rating inflation by regressing each team's rating toward their
  #' conference mean at season end. Independent teams regress to overall FBS mean.
  #' Non-FBS teams reset to initial rating.
  #'
  #' @param conf_df Data frame with `school` and `conf` columns
  #' @param df Data frame with `school` and `rating` columns
  #' @param regress_val Fraction to regress toward mean (0–1, default = REGRESS_AMOUNT)
  #'
  #' @return Data frame with regressed ELO ratings
  
  df |> 
    dplyr::left_join(conf_df, by = "school") |>
    dplyr::group_by(conf) |>
    dplyr::mutate(mean_conf_elo = mean(rating, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      fbs = !is.na(conf),
      overall_mean = mean(rating, na.rm = TRUE),
      regress_rating = dplyr::case_when(
        conf != "Ind" ~ rating + regress_val * (mean_conf_elo - rating),
        conf == "Ind" ~ rating + regress_val * (overall_mean - rating),
        is.na(conf) ~ INITIAL_RATING
      ),
      regressed = 1
    ) |>
    dplyr::select(school, rating, regress_rating, mean_conf_elo, overall_mean, regressed) |>
    dplyr::arrange(desc(rating))
}