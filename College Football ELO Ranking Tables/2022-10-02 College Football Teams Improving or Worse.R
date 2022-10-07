# creating a table to rank college football teams that have either improved or done worse since the beginning of the season
# which teams have gained or lost more than 5% of their pre-season ELO?

library(gt)
library(htmltools)
library(glue)
library(tidyverse)

options(scipen = 999)

setwd('~/GitHub/Sports/College Football ELO Ranking Tables')

# load the elo model and schedule data
full_elo_df <- read.csv('~/GitHub/Sports/College Football ELO Model/Data/FullELODF.csv') %>% select(-1)
conferences <- read.csv('~/GitHub/Sports/College Football Schedule Scrapping/Data/Conferences.csv') %>% select(-1)
fbs_schedule <- read.csv('~/GitHub/Sports/College Football Schedule Scrapping/Data/FBS Full Schedule.csv') %>% select(-1)

season_var <- 2022

# what is the latest week in the season selected? 
season_wk <- full_elo_df %>%
  filter(season == season_var) %>%
  filter(wk == max(wk)) %>%
  select(wk) %>%
  slice(1) %>%
  as.numeric()

season_label <- season_wk-1

# variables for the gt table
up_arrow <- "<span style=\"color:#4292c6\">&#9650;</span>"
down_arrow <- "<span style=\"color:#ef6548\">&#9660;</span>"
no_arrow <- "<span style=\"color:#fed976\">&#9644;</span>"

plus_symbol <- function(.x) {
  glue::glue("+{.x}")
}

less_than_100 <- function(.x) {
  glue::glue("+{.x}%")
}

plus_percent <- function(.x) {
  glue::glue("+{.x}%")
}

percent_symbol <- function(.x) {
  glue::glue("{.x}%")
}

# what games have been played for each team and who is next?
latest_results <- fbs_schedule %>%
  filter(wk == season_wk,
         season == season_var) %>%
  select(wk, 
         school,
         pts,
         opp,
         opponent,
         location) %>%
  filter(!is.na(pts)) %>%
  mutate(result = ifelse(pts > opp, 'W', 'L'),
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
  inner_join(conferences,
             by = c('season' = 'season',
                    'team_a' = 'school')) %>%
  filter(season == season_var) %>%
  group_by(team_a) %>%
  filter(wk == min(wk)) %>%
  ungroup() %>%
  mutate(pre_elo = case_when(pts > opp ~ elo_a-elo_adj,
                             pts < opp ~ elo_a+elo_adj)) %>%
  mutate(wk = 0) %>%
  select(season,
         wk,
         team_a,
         elo_a = pre_elo) %>%
  mutate(rank = dense_rank(desc(elo_a)),
         wins = 0,
         loses = 0)

pre_school <- pre_ratings %>%
  select(team_a,
         pre_rating = elo_a,
         pre_rank = rank)

season_wk_ranks <- full_elo_df %>%
  filter(season == season_var) %>%
  inner_join(conferences,
             by = c('season' = 'season',
                    'team_a' = 'school')) %>%
  select(season,
         wk,
         team_a,
         elo_a,
         elo_adj) %>%
  arrange(team_a,
          wk) %>%
  group_by(team_a) %>%
  complete(wk = seq.int(1, season_wk, 1)) %>%
  bind_rows(pre_ratings) %>%
  arrange(wk) %>%
  fill(season) %>%
  fill(elo_a) %>%
  mutate(wins = ifelse(elo_a > lag(elo_a) & wk != 0, 1, 0),
         loses = ifelse(elo_a < lag(elo_a) & wk != 0,1, 0),
         elo_adj = elo_a - lag(elo_a)) %>%
  fill(loses, .direction = 'up') %>%
  fill(wins, .direction = 'up') %>%
  mutate(roll_wins = cumsum(wins),
         roll_loses = cumsum(loses)) %>%
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
  unite(record, c('roll_wins', 'roll_loses'), sep = '-') %>%
  left_join(latest_results,
            by = c('team_a' = 'school')) %>%
  left_join(next_match,
            by = c('team_a' = 'school')) %>%
  mutate(result_label = ifelse(is.na(result_label), 'Did not play', result_label),
         next_label = ifelse(is.na(next_label), 'Does not play', next_label)) %>%
  select(wk,
         team_a,
         conf,
         rank,
         jump,
         record,
         elo_a,
         elo_adj,
         result_label,
         next_label) 

# which teams have either gained or lost 5% of their pre-season ELO ratings?
team_improvement <- season_wk_ranks %>%
  filter(wk == season_wk) %>%
  inner_join(pre_school) %>%
  mutate(improvement = (elo_a-pre_rating)/pre_rating,
         rank_change = pre_rank-rank) %>%
  arrange(desc(improvement)) %>%
  select(team_a,
         record,
         conf,
         improvement,
         current_elo = elo_a,
         pre_elo = pre_rating,
         rank,
         rank_change,
         result_label,
         next_label) %>%
  mutate(current_elo = round(current_elo, 1),
         pre_elo = round(pre_elo, 1),
         improvement = round(improvement, 3)*100) %>%
  filter(improvement >= 5 | improvement <= -5,
         dense_rank(desc(improvement)) <=10 | dense_rank((improvement)) <=10) %>%
  rename(School = team_a,
         Record = record,
         `Conference & Div` = conf,
         `Percent Change` = improvement,
         `Current ELO` = current_elo,
         `Pre-Season ELO` = pre_elo,
         Rank = rank,
         `Jump from Week #1` = rank_change,
         `Game Result` = result_label,
         `Next Match` = next_label) 

# the final table
team_improvement %>%
  gt() %>%
  tab_header(
    title = md("**Which College Football Teams Improved or Got Worse Since Week #1?**"),
    subtitle = glue("Top & bottom 10 teams based on ELO Percent Change from Week #1 | As of Week #{season_label}")
  ) %>%
  tab_source_note(
    source_note = glue('ELO ratings score each team based on factors such as home-field advantage, margin of victory, and quality of opponent. At the end of each season, school ratings regress partially to the value of their respective conference.')
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
    locations = cells_body(vars(`School`, `Percent Change`))
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
  fmt("Percent Change", 
      rows = `Percent Change` > 0, 
      fns = plus_percent) %>%
  fmt("Percent Change", 
      rows = `Percent Change` < 0, 
      fns = percent_symbol) %>%
  fmt("Jump from Week #1", 
      rows = `Jump from Week #1` > 0, 
      fns = plus_symbol) %>%
  tab_options(table.font.names = "IBM Plex Sans",
              table.font.size = 12) %>%
  cols_align(
    align = c("left"),
    columns = c(`Jump from Week #1`, `Pre-Season ELO`)
  ) %>%
  text_transform(
    locations = cells_body(
      columns = `Jump from Week #1`,
      rows = `Jump from Week #1` > 0),
    fn = function(x) paste(x, up_arrow)
  ) %>%
  text_transform(
    locations = cells_body(
      columns = `Jump from Week #1`,
      rows = `Jump from Week #1` < 0),
    fn = function(x) paste(x, down_arrow)
  ) %>%
  text_transform(
    locations = cells_body(
      columns = `Jump from Week #1`,
      rows = `Jump from Week #1` == 0),
    fn = function(x) paste(x, no_arrow)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F9E3D6")
    ),
    locations = cells_body(
      columns = `Percent Change`,
      rows = `Percent Change` < 0
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#c6dbef")
    ),
    locations = cells_body(
      columns = `Percent Change`,
      rows = `Percent Change` > 0
    )
  ) %>% 
  gtsave("Improved or Worse Teams.png")