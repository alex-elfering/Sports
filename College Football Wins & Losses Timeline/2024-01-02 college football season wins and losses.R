# college football season stats

library(tidyverse)
library(tidylog)

school_games <- read.csv("C:/Users/alexe/OneDrive/Documents/Sports Analysis/school_games_conf.csv") |> select(-X)

school_matches <- school_games |>
  select(season,
         wk,
         date,
         school,
         school_conf,
         opponent,
         opponent_conf,
         location,
         school_pts,
         opponent_pts) |>
  arrange(season,
          wk) |>
  mutate(win = ifelse(school_pts > opponent_pts, 1, 0),
         lose = ifelse(school_pts < opponent_pts, 1, 0),
         tie = ifelse(school_pts == opponent_pts, 1, 0),
         margin = school_pts-opponent_pts,
         close_win = ifelse(win == 1 & (margin > 0 & margin <= 7), 1, 0),
         close_loss = ifelse(lose == 1 & (margin < 0 & margin >= -7), 1, 0))

latest_season <- school_matches |>
  distinct(season) |>
  filter(season == max(season)) |>
  as.numeric()

school_summary_season <- school_matches |>
  group_by(season,
           school,
           school_conf) |>
  summarise(pts = sum(school_pts, na.rm = T),
            opp_pts = sum(opponent_pts, na.rm = T),
            wins = sum(win, na.rm = T),
            losses = sum(lose, na.rm = T),
            ties = sum(tie, na.rm = T),
            close_wins = sum(close_win, na.rm = T),
            close_losses = sum(close_loss, na.rm = T)) |>
  ungroup() |>
  filter(!is.na(school_conf)) |>
  filter(season >= 1950) |>
  mutate(winning_season = ifelse(wins/(wins + losses + ties) > 0.5,1,0),
         losing_season = ifelse(winning_season == 0, 1, 0)) |>
  pivot_longer(cols = c('wins', 'losses','ties','winning_season','losing_season','pts', 'opp_pts', 'close_wins', 'close_losses')) |>
  mutate(val_diver = ifelse(name %in% c('wins', 'close_wins'), value, value*-1))

schools_currently <- school_summary_season |>
  filter(season == latest_season) |>
  distinct(school)

latest_school_stats <- school_summary_season |>
  inner_join(schools_currently)

short_school_matches <- school_matches |>
  inner_join(schools_currently) |>
  mutate(conference_match = school_conf == opponent_conf) |>
  select(season,
         wk,
         date,
         school,
         opponent,
         school_pts,
         opponent_pts,
         margin,
         win,
         lose,
         tie,
         conference_match) |>
  pivot_longer(col = c('win', 'lose', 'tie')) |>
  filter(value > 0) |>
  mutate(name = gsub('win', 'wins', name),
         name = gsub('lose', 'losses', name),
         name = gsub('tie', 'ties', name))

write.csv(short_school_matches,"C:/Users/alexe/OneDrive/Documents/Sports Analysis/short_school_matches.csv")
write.csv(latest_school_stats,"C:/Users/alexe/OneDrive/Documents/Sports Analysis/school_summary_season.csv")