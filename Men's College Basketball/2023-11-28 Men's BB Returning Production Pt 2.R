# re: returning production
# find a way to control for players who started at least 'n' games, or played at least 'n' minutes

library(tidyverse)
library(lubridate)
library(tidylog)
library(janitor)

#roster ----
player_roster <- read.csv('C:/Users/alexe/OneDrive/Documents/Sports Analysis/MBB Returning Production/player roster.csv')

school_name_capitalized <- player_roster |>
  distinct(schl) |>
  mutate(school_name = case_when(schl == 'loyola-il' ~ 'Loyola (IL)',
                                 schl == 'miami-oh' ~ 'Miami (OH)',
                                 schl == 'illinois-chicago' ~ 'Illinois-Chicago',
                                 schl == 'iupui' ~ 'IUPUI',
                                 schl == 'nebraska-omaha' ~ 'Omaha',
                                 TRUE ~ tools::toTitleCase(gsub('-', ' ', schl))))

clean_roster <- player_roster |>
  left_join(school_name_capitalized) |>
  select(player,
         pos,
         season,
         school_name,
         class)

# player index  ----
player_index <- read.csv('C:/Users/alexe/OneDrive/Documents/Sports Analysis/MBB Returning Production/player game stats.csv')

clean_player_index <- player_index |>
  mutate(date = ymd(date),
         season = ifelse(month(date) >= 1 & month(date) <= 4, year(date)-1, year(date))) |>
  filter(schl != 'Schl') |>
  select(player,
         season,
         schl,
         date,
         opp,
         gs,
         mp,
         fg,
         fga,
         x2p,
         x2pa,
         x3p,
         x3pa,
         trb,
         ast,
         stl,
         blk,
         pts,
         pf) 

player_games <- clean_player_index |>
  distinct(player,
           schl,
           season,
           gs,
           mp,
           date) |>
  group_by(player,
           schl,
           season) |>
  #mutate(mp = as.numeric(mp),
  #       gs = as.numeric(gs)) |>
  summarise(total_games = n_distinct(date),
         total_starts = sum(as.numeric(gs)),
         total_min = sum(as.numeric(mp))) |>
  ungroup |>
  mutate(next_season = season + 1)

player_stats <- clean_player_index |>
  select(-gs,
         -mp) |>
  pivot_longer(cols = -c('player',
                         'schl',
                         'season',
                         'date',
                         'opp'),
               names_to = 'stat') |>
  mutate(value = as.numeric(value)) |>
  group_by(player,
           season,
           schl,
           stat) |>
  summarise_if(is.numeric, sum) |>
  ungroup() |>
  group_by(schl,
           stat,
           season) |>
  mutate(total_value = sum(value),
         pct_total = value/total_value) |>
  ungroup() |>
  #arrange(desc(pct_total)) |>
  mutate(next_season = season + 1)