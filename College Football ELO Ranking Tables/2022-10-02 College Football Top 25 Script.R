# ELOレーティング上位25チーム

library(tidyverse)
library(glue)
library(gt)
library(htmltools)
#install.packages('webshot2')

options(scipen = 999)

setwd('C:/Users/alexe/OneDrive/Desktop')

# load the elo model and schedule data
full_elo_df <- read.csv('C:/Users/alexe/OneDrive/Desktop/FullELODF.csv') %>% select(-1)
conferences <- read.csv('C:/Users/alexe/OneDrive/Desktop/Conferences.csv') %>% select(-1)
fbs_schedule <- read.csv('C:/Users/alexe/OneDrive/Desktop/FBS Full Schedule.csv') %>% select(-1)

season_var <- 1950

# what is the latest week in the season selected? 
season_wk <- full_elo_df %>%
  filter(season == season_var) %>%
  filter(wk == max(wk)) %>%
  select(wk) %>%
  slice(1) %>%
  as.numeric()

#season_wk <-16

season_label <- season_wk-1

# variables for the gt table
up_arrow <- "<span style=\"color:#4292c6\">&#9650;</span>"
down_arrow <- "<span style=\"color:#ef6548\">&#9660;</span>"
no_arrow <- "<span style=\"color:#fed976\">&#9644;</span>"

plus_symbol <- function(.x) {
  glue::glue("+{.x}")
}

# what games have been played for each team and who is next?
latest_results <- fbs_schedule %>%
 # filter(school == 'Georgia') %>%
  filter(wk == season_wk,
         season == season_var) %>%
  select(wk, 
         school,
         pts,
         opp,
         opponent,
         location) %>%
  filter(!is.na(pts)) %>%
  mutate(result = case_when(pts == opp ~ 'T',
                            pts > opp ~ 'W',
                            pts < opp ~ 'L'),
         #result = ifelse(pts > opp, 'W', 'L'),
         result_label = paste(result, ' ', location, ' ', opponent, ' ', pts, '-', opp, sep = '')) %>%
  select(school,
         result_label)

next_match <- fbs_schedule %>%
  filter(wk == season_wk + 1,
         season == season_var) %>%
  select(wk, 
         school,
         pts,
         opp,
         opponent,
         location) %>%
  mutate(next_label = paste(location, opponent, sep = ' ')) %>%
  select(school,
         next_label)

# calculate rankings by week for the season
pre_ratings <- full_elo_df %>%
  #filter(team_a == 'Colorado') %>%
  inner_join(conferences,
             by = c('season' = 'season',
                    'team_a' = 'school')) %>%
  filter(season == season_var) %>%
  group_by(team_a) %>%
  filter(wk == min(wk)) %>%
  ungroup() %>%
  mutate(pre_elo = case_when(pts > opp ~ elo_a-elo_adj,
                             pts < opp ~ elo_a+elo_adj,
                             pts == opp ~ elo_a)) %>%
  mutate(wk = 0) %>%
  select(season,
         wk,
         team_a,
         elo_a = pre_elo) %>%
  mutate(rank = dense_rank(desc(elo_a)),
         wins = 0,
         loses = 0,
         ties = 0)

season_wk_ranks <- full_elo_df %>%
  filter(season == season_var) %>%
  select(season,
         wk,
         team_a,
         elo_a,
         elo_adj,
         pts, 
         opp) %>%
  arrange(team_a,
          wk) %>%
  group_by(team_a) %>%
  complete(wk = seq.int(1, season_wk, 1)) %>%
  bind_rows(pre_ratings) %>%
  arrange(wk) %>%
  fill(season) %>%
  fill(elo_a) %>%
  mutate(wins = ifelse(pts > opp & wk != 0, 1, 0),
         loses = ifelse(pts < opp & wk != 0,1, 0),
         ties = ifelse(pts == opp & wk != 0,1, 0),
         elo_adj = elo_a - lag(elo_a)) %>%
  mutate(wins = ifelse(is.na(wins), 0, wins),
         loses = ifelse(is.na(loses), 0 , loses),
         ties = ifelse(is.na(ties), 0, ties)) %>%
  mutate(roll_wins = cumsum(wins),
         roll_loses = cumsum(loses),
         roll_ties = cumsum(ties)) %>%
  ungroup() %>%
  group_by(wk) %>%
  mutate(rank = dense_rank(desc(elo_a))) %>%
  ungroup() %>%
  group_by(team_a) %>%
  mutate(prev_rank = lag(rank),
         jump = (rank-lag(rank) ),
         jump = ifelse(jump < 0, jump*-1, jump*-1)) %>%
  ungroup() %>%
  inner_join(conferences,
             by = c('season' = 'season',
                    'team_a' = 'school')) %>%
  arrange(wk,
          rank) %>%
  unite(conf, c('conf', 'div'), sep = ' ', na.rm = TRUE) %>%
  mutate(roll_ties = ifelse(roll_ties == 0, NA, roll_ties)) %>%
  unite(record, c('roll_wins', 'roll_loses', roll_ties), na.rm = TRUE, sep = '-') %>%
  #filter(team_a == 'Michigan State') %>%
  select(wk,
         team_a,
         conf,
         rank,
         jump,
         record,
         elo_a,
         elo_adj) 

# what teams are almost in the rankings?
almost <- season_wk_ranks %>%
  filter(wk == season_wk) %>%
  filter(rank > 25 & rank <= 35) %>%
  select(team_a) %>%
  mutate(index = row_number()) %>%
  pivot_wider(names_from = index,
              values_from = team_a) %>%
  unite(team_almost, c(1:9), sep = ', ')

teams_almost <- as.character(almost$team_almost)

# what teams are in the rankings?
teams_ranked <- season_wk_ranks %>%
  filter(wk == season_wk) %>%
  filter(rank <= 25) %>%
  left_join(latest_results,
            by = c('team_a' = 'school')) %>%
  left_join(next_match,
            by = c('team_a' = 'school')) %>%
  mutate(result_label = ifelse(is.na(result_label), 'Did not play', result_label),
         next_label = ifelse(is.na(next_label), 'Does not play', next_label)) %>%
  select(rank,
         team_a,
         conf,
         jump,
         record,
         elo_a,
         elo_adj,
         result_label,
         next_label) %>%
  mutate(elo_a = round(elo_a, 1),
         elo_adj = round(elo_adj, 1)) %>%
  rename(School = team_a,
         Record = record,
         `Conference & Div` = conf,
         Rank = rank,
         `Jump from Prior Week` = jump,
         `Current ELO` = elo_a,
         `Change from Prior Week` = elo_adj,
         `Game Result` = result_label,
         `Next Match` = next_label) 

teams_ranked %>% as.data.frame()

teams_ranked %>%
  gt() %>%
  tab_header(
    title = md("**カレッジフットボール**"),
    subtitle = glue("ELOレーティング上位25チーム | {season_var} Season as of Week #{season_label}")
  ) %>%
  tab_source_note(
    source_note = glue('Almost: {teams_almost}')
  ) %>%
  tab_source_note(
    source_note = glue('ELO ratings score each team based on factors such as home-field advantage, margin of victory, and quality of opponent. At the end of each season, school ratings regress partially to the value of their respective conference. Teams new to FBS begin with an ELO rating of 1500.')
  ) %>%
  tab_source_note(
    source_note = "Code by Alex Elfering | Source: College Football Reference | Model Inspired by FiveThirtyEight"
  ) %>%
  tab_style(
    style = cell_text(
      color = "gray75"
    ),
    locations = cells_body(vars(`Conference & Div`))
  ) %>%
  tab_style(
    style = cell_text(
      color = "gray75"
    ),
    locations = cells_body(
      columns = c(`Game Result`),
      rows = `Game Result` == 'Did not play')
  ) %>%
  tab_style(
    style = cell_text(
      color = "gray75"
    ),
    locations = cells_body(
      columns = c(`Next Match`),
      rows = `Next Match` == 'Does not play')
  ) %>%
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_body(vars(`School`, `Rank`))
  ) %>%
  tab_options(
    column_labels.border.top.style = "none",
    table.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    column_labels.border.bottom.width = 1,
    column_labels.border.bottom.color = "#334422",
    table_body.border.top.style = "none",
    table_body.border.bottom.color = "#0000001A",
    data_row.padding = px(7)
  ) %>%
  tab_style(
    style = cell_text(
      size = px(12),
      weight = "bold",
      transform = "uppercase"
    ),
    locations = cells_column_labels(everything())
  ) %>%
  cols_align(
    align = c("left"),
    columns = `Change from Prior Week`
  ) %>%
  tab_style(
    style = cell_text(
      size = px(12)
    ),
    locations = cells_body(everything())
  ) %>%
  text_transform(
    locations = cells_body(
      columns = `Jump from Prior Week`,
      rows = `Jump from Prior Week` > 0),
    fn = function(x) paste(x, up_arrow)
  ) %>%
  text_transform(
    locations = cells_body(
      columns = `Jump from Prior Week`,
      rows = `Jump from Prior Week` < 0),
    fn = function(x) paste(x, down_arrow)
  ) %>%
  text_transform(
    locations = cells_body(
      columns = `Jump from Prior Week`,
      rows = `Jump from Prior Week` == 0),
    fn = function(x) paste(x, no_arrow)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F9E3D6")
    ),
    locations = cells_body(
      columns = `Change from Prior Week`,
      rows = `Change from Prior Week` < 0
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#c6dbef")
    ),
    locations = cells_body(
      columns = `Change from Prior Week`,
      rows = `Change from Prior Week` > 0
    )
  ) %>%
  tab_options(table.font.names = "Noto Sans JP",
              table.font.size = 12) %>%
  fmt("Change from Prior Week", 
      rows = `Change from Prior Week` > 0, 
      fns = plus_symbol) %>%
  fmt("Jump from Prior Week", 
      rows = `Jump from Prior Week` > 0, 
      fns = plus_symbol)




