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
         pts) |>
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

clean_player_index |>
  distinct(schl)

#roster ----
player_roster <- read.csv('C:/Users/alexe/OneDrive/Documents/Sports Analysis/MBB Returning Production/player roster.csv')

school_name_capitalized <- player_roster |>
  distinct(schl) |>
  mutate(school_name = case_when(schl == 'loyola-il' ~ 'Loyola (IL)',
                                 schl == 'miami-oh' ~ 'Miami (OH)',
                                 schl == 'illinois-chicago' ~ 'Illinois-Chicago',
                                 TRUE ~ tools::toTitleCase(gsub('-', ' ', schl))))

clean_roster <- player_roster |>
  left_join(school_name_capitalized) |>
  select(player,
         season,
         school_name,
         class)


