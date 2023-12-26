# what are the greatest turnarounds in College Football over a 10-year period?

# functions for gt borrowed from (thank you!): https://github.com/malcolmbarrett/malco.io/blob/master/content/post/2020-05-16-replicating-an-nyt-table-of-swedish-covid-deaths-with-gt/index.Rmd

library(ggplot2)
library(tidyverse)
library(zoo)
library(gt)
library(data.table)
library(glue)

school_games <- read.csv("C:/Users/alexe/OneDrive/Documents/Sports Analysis/school_games_conf.csv")

n <- 10
top_n <- 15

school_season_results <- school_games |>
  filter(!is.na(school_conf)) |>
  select(season,
         school,
         school_conf,
         opponent,
         opponent_conf,
         date,
         wk,
         school_pts,
         opponent_pts) |>
  filter(!is.na(school_pts)) |>
  mutate(wins = ifelse(school_pts > opponent_pts, 1, 0),
         loses = ifelse(school_pts < opponent_pts, 1, 0),
         ties = ifelse(school_pts == opponent_pts, 1, 0),
         games = 1,
         margin = school_pts - opponent_pts) |>
  group_by(season,
           school,
           school_conf) |>
  summarise(wins = sum(wins, na.rm = T),
            losses = sum(loses, na.rm = T),
            ties = sum(ties, na.rm = T),
            games = sum(games)) |>
  ungroup()

roll_school_trends <- school_season_results |>
  group_by(school) |>
  complete(season = seq.int(min(season), max(season), 1))  |>
  mutate(winning_season = ifelse(wins/(wins + losses + ties) > 0.5, 1, 0),
         losing_season = ifelse(wins/(wins + losses + ties) <= 0.5, 1, 0),
         winning_season_streak = rowid(rleid(winning_season)) * winning_season,
         losing_season_streak = rowid(rleid(losing_season)) * losing_season) |>
  mutate(roll_wins = rollapplyr(wins, n, sum, partial = T),
         roll_losses = rollapplyr(losses, n, sum, partial = T),
         roll_ties = rollapplyr(ties, n, sum, partial = T),
         roll_games = rollapplyr(games, n, sum, partial = T),
         roll_win_pct = roll_wins/roll_games) |>
  filter(row_number() > n,
         season > 1950) |>
  mutate(roll_ties_label = ifelse(roll_ties == 0, NA, roll_ties),
         roll_wins_label = roll_wins,
         roll_losses_label = roll_losses,
         roll_win_pct_change = (roll_win_pct-lag(roll_win_pct, n = n)),
         roll_prior_win_pct = lag(roll_win_pct, n = n),
         min_season = rollapplyr(season, n, min, partial = T)) |>
  ungroup()

school_trends <- roll_school_trends |>
  group_by(school) |>
  unite(roll_label, c('roll_wins_label', 'roll_losses_label', 'roll_ties_label'), sep = '-', na.rm = T) |>
  mutate(prior_record = lag(roll_label, n = n)) |>
  arrange(desc(roll_win_pct_change)) |>
  filter(row_number() == min(row_number())) |>
  ungroup() |>
  as.data.frame() |>
  filter(dense_rank(desc(roll_win_pct_change)) <= top_n) |>
  mutate(prior_begin = min_season-n,
         prior_end = season-n) |>
  unite(period, c('min_season', 'season'), sep = '-') |>
  unite(prior_period, c('prior_begin', 'prior_end'), sep = '-') |>
  mutate(roll_win_pct_change = round(roll_win_pct_change*100,1),
         roll_prior_win_pct = round(roll_prior_win_pct*100,1),
         roll_win_pct = round(roll_win_pct*100,1)) |>
  select(school,
         school_conf,
         period,
         roll_label,
         roll_win_pct,
         prior_period,
         prior_record,
         roll_prior_win_pct,
         roll_win_pct_change)

plus_percent <- function(.x) {
  glue::glue("+{.x}%")
}

percent_label <- function(.x) {
  glue::glue("{.x}%")
}

school_trends |>
  gt() |>
  fmt("roll_win_pct_change", fns = plus_percent) |>
  fmt("roll_prior_win_pct", fns = percent_label) |>
  fmt("roll_win_pct", fns = percent_label) |>
  tab_style(
    style = cell_text(
      size = px(11),
      color = "#999",
      font = "Arial",
      transform = "uppercase"
    ),
    locations = cells_column_labels(everything())
  ) |>
  tab_style(
    style = cell_text(
      size = px(12),
      color = "black",
      font = "arial"
    ),
    locations = cells_body(everything())
  ) |>
  tab_style(
    style = cell_text(
      size = px(12),
      color = "black",
      font = "consolas"
    ),
    locations = cells_body(columns = c('roll_label',
                                       'prior_record'))
  ) |>
  tab_style(
    style = cell_text(
      size = px(12),
      color = "black",
      font = "consolas",
      weight = 'bold'
    ),
    locations = cells_body(columns = c('roll_win_pct',
                                       'roll_prior_win_pct',
                                       'roll_win_pct_change'))
  ) |>
  tab_header(
    title = md(glue("**{top_n} Biggest Turnarounds in FBS College Football Turnarounds**")),
    subtitle = glue("Comparing Winning Percentage over {n} seasons compared to {n} seasons prior")
  ) |>
  cols_label(
    school = "school",
    school_conf = "conference",
    period = "period",
    roll_label = "record",
    roll_win_pct = "winning %",
    prior_period = "period",
    prior_record = "record",
    roll_prior_win_pct = 'winning %',
    roll_win_pct_change = '% Diff'
  ) |> 
  tab_spanner(
    label = md(glue("{n} Seasons Prior")),
    columns = c("prior_period",
                "prior_record",
                "roll_prior_win_pct")
  ) |>
  fmt("roll_win_pct_change", fns = plus_percent) |>
  tab_style(
    style = cell_text(
      size = px(11),
      color = "black",
      font = "Arial",
      transform = "uppercase"
    ),
    locations = cells_column_spanners(everything())
  )  |>
  tab_footnote(footnote = md("Source: College-Football-Reference")) |>
  tab_footnote(footnote = md("Note: Excludes FCS and non-Div I matches")) |>
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = "gray80",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(columns = c('school_conf',
                                       'roll_win_pct',
                                       'roll_prior_win_pct'))
  )
