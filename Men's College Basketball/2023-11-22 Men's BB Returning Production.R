# men's college basketball returning production
# examining midwest(-adjacent) schools

library(tidyverse)
library(lubridate)
library(tidylog)
library(janitor)

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
         x2p,
         x3p,
         trb,
         ast,
         stl,
         blk,
         pts) 

player_metric_summary <- clean_player_index |>
  pivot_longer(cols = -c('player',
                         'season',
                         'schl',
                         'date',
                         'opp'),
               names_to = 'metric',
               values_to = 'values') |>
  mutate(values = as.numeric(values)) |>
  group_by(player,
           schl,
           season,
           metric) |>
  summarise(values = sum(values, na.rm = T))

total_games_player <- clean_player_index |>
  group_by(season,
           player) |>
  summarise(total_games = n())
  
  
#roster ----
player_roster <- read.csv('C:/Users/alexe/OneDrive/Documents/Sports Analysis/MBB Returning Production/player roster.csv')

school_name_capitalized <- player_roster |>
  distinct(schl) |>
  mutate(school_name = case_when(schl == 'loyola-il' ~ 'Loyola (IL)',
                                 schl == 'miami-oh' ~ 'Miami (OH)',
                                 schl == 'illinois-chicago' ~ 'Illinois-Chicago',
                                 schl == 'iupui' ~ 'IUPUI',
                                 TRUE ~ tools::toTitleCase(gsub('-', ' ', schl))))

clean_roster <- player_roster |>
  left_join(school_name_capitalized) |>
  select(player,
         season,
         school_name,
         class)

# what are each win's w-l record per season ----
schl_wl_record <- player_index |>
  mutate(date = ymd(date),
         season = ifelse(month(date) >= 1 & month(date) <= 4, year(date)-1, year(date))) |>
  select(season,
         schl,
         result = x_2,
         date) |>
  filter(season != 2023) |>
  group_by(season,
           schl,
           result) |>
  summarise(games = n_distinct(date)) |>
  ungroup()

# what is the returning production  ----

player_ratio_metric <- player_metric_summary |>
  group_by(schl,
           season,
           metric) |>
  mutate(total = sum(values),
         pct_total = values/total) |>
  ungroup() |>
  left_join(total_games_player) |>
  mutate(avg_val = values/total_games,
         next_season = season + 1) |>
  filter(season != 2023)


mbb_returning_production <- player_ratio_metric |>
  inner_join(clean_roster,
             by = c('next_season' = 'season',
                    'player' = 'player',
                    'schl' = 'school_name')) |>
  rename(next_class = class) |>
  left_join(clean_roster,
             by = c('season' = 'season',
                    'player' = 'player',
                    'schl' = 'school_name')) |>
  filter(!is.na(class))






