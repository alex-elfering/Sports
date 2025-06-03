# ELO functions

library(data.table)
library(tidyverse)
library(ggplot2)
library(rvest)
library(stringi)
library(glue)

# data sources  ----
winning_games <- read.csv("C:/Users/alexe/OneDrive/Documents/Sports Analysis/2025 CFB Data Modeling/Data Sources/unique cfb games.csv") |>
  distinct() |>
  rename(location = var_7,
         school = winner,
         opponent = loser) |>
  select(-X) |>
  filter(!is.na(pts))

conf_df <- read.csv("C:/Users/alexe/OneDrive/Documents/Sports Analysis/2025 CFB Data Modeling/Data Sources/school conferences.csv") |> arrange(season) |> select(season, school, conf, div)

# functions ----
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

elo_week_update <- function(df, use_elo_df, week_int, team_adv = 75, k_val = 30) {
  #' Update ELO Ratings for One Week
  #'
  #' @param df Game-level data (must include 'wk', 'school', 'opponent', 'pts', 'opp', 'wins', 'loses', 'ties', 'location')
  #' @param use_elo_df Data frame of current ELO ratings (must include 'school' and 'rating')
  #' @param week_int Integer for the week number to calculate
  #' @param team_adv Home field advantage (default = 75)
  #' @param k_val ELO k-factor (default = 30)
  #'
  #' @return A data frame with updated team and opponent ELOs and match results
  
  required_cols <- c("wk", "school", "opponent", "pts", "opp", "wins", "loses", "ties", "location")
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns in df")
  }
  
  df |>
    dplyr::filter(wk == week_int) |>
    dplyr::left_join(use_elo_df, by = c("school" = "school")) |>
    dplyr::rename(school_elo = rating) |>
    dplyr::left_join(use_elo_df, by = c("opponent" = "school")) |>
    dplyr::rename(opp_elo = rating) |>
    dplyr::mutate(
      school_elo_adj = dplyr::case_when(
        location == '' ~ school_elo + team_adv,
        location == '@' ~ school_elo - team_adv,
        location == 'N' ~ school_elo
      ),
      opp_elo_adj = dplyr::case_when(
        location == '@' ~ opp_elo + team_adv,
        location == '' ~ opp_elo - team_adv,
        location == 'N' ~ opp_elo
      ),
      m = (school_elo_adj - opp_elo_adj) / 400,
      elo_diff = abs(school_elo_adj - opp_elo_adj),
      ps = round(elo_diff / 25, 1),
      p_opponent = 1 / (1 + 10^m),
      p_team = 1 - p_opponent,
      margin_abs = abs(pts - opp),
      mov_mult = log(margin_abs + 1) * (2.2 / (elo_diff * 0.001 + 2.2)),
      elo_adj = (k_val * mov_mult) * (1 - p_team),
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

regress_ratings <- function(conf_df, df, regress_val = 0.3) {
  #' Regress Ratings Toward Conference Mean
  #'
  #' @param conf_df Data frame with `school` and `conf` columns
  #' @param df Data frame with `school` and `rating` columns
  #' @param regress_val Fraction to regress (0–1)
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
        is.na(conf) ~ 1500
      ),
      regressed = 1
    ) |>
    dplyr::select(school, rating, regress_rating, mean_conf_elo, overall_mean, regressed) |>
    dplyr::arrange(desc(rating))
}