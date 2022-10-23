# college football bowl predictions - which teams are likely to go at least 0.500 or win a certain number of games?

# libraries and data loading  ----
library(tidyverse)
library(data.table)
library(rvest)
library(glue)
library(stringi)

# if the unique id vector already exists, then don't run it
if(exists('list_unique') == F){
  source('~/GitHub/Sports/College Football Bowl Predictions/2022-10-22 unique game ids.R')
}else{
  print('unique id fields exist already')
  }
  
source('~/GitHub/Sports/College Football Bowl Predictions/2022-10-22 probability functions.R')

fbs_schedule <- read.csv('~/GitHub/Sports/College Football Schedule Scrapping/Data/FBS Full Schedule.csv') %>% select(-1)
full_elo_df <- read.csv('~/GitHub/Sports/College Football ELO Model/Data/FullELODF.csv') %>% select(-1)
conferences <- read.csv('~/GitHub/Sports/College Football Schedule Scrapping/Data/Conferences.csv') %>% select(-1)

# variables and functions ----
season_vari <- 2022
n_times <- 10000

# establish data frames to create the forecast  ----
filter_schedule <- fbs_schedule %>%
  filter(season == season_vari) %>%
  mutate(unique_id = paste0(season, wk, school, opponent)) %>%
  mutate(unique_id = gsub(' ', '', unique_id)) %>%
  filter(unique_id %in% list_unique) %>%
  select(-unique_id)

pre_ratings <- full_elo_df %>%
  inner_join(conferences,
             by = c('season' = 'season',
                    'team_a' = 'school')) %>%
  filter(season == season_vari) %>%
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

season_wk_ranks <- full_elo_df %>%
  filter(season == season_vari) %>%
  select(season,
         wk,
         team_a,
         elo_a,
         elo_adj) %>%
  arrange(team_a,
          wk) %>%
  group_by(team_a) %>%
  complete(wk = seq.int(1, 9, 1)) %>%
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
  select(wk,
         team_a,
         conf,
         rank,
         jump,
         record,
         elo_a,
         elo_adj) 

season_ratings <- season_wk_ranks %>%
  #filter(team_a == 'Nebraska') %>%
  select(wk,
         school = team_a,
         rating= elo_a)

season_results <- fbs_schedule %>%
  filter(season == season_vari) %>%
  mutate(wins = ifelse(pts > opp, 1, 0),
         loses = ifelse(pts < opp, 1, 0)) %>%
  group_by(school,
           wk) %>%
  summarise(wins = sum(wins),
            loses = sum(loses)) %>%
  ungroup() %>%
  group_by(school) %>%
  complete(wk = seq(1, 9)) %>%
  replace(is.na(.), 0) %>%
  mutate(roll_wins = cumsum(wins),
         roll_loses = cumsum(loses)) %>%
  ungroup() %>%
  select(school,
         wk,
         c_wins = roll_wins,
         c_lose = roll_loses)

max_forecast_wk <- full_elo_df %>%
  filter(season == season_vari) %>%
  distinct(wk) %>%
  summarise(wk = max(wk)) %>%
  as.numeric()

# the forecast  ----
week_forecast <- list()
total_wins_list <- list()
six_wins_list <- list()
for(i in 1:max_forecast_wk){
  
  current_week <- season_results %>%
    filter(wk == i) %>%
    select(-wk)
  
  filter_ratings <- season_ratings %>%
    filter(wk == i) %>%
    select(-wk)
  
  test <- filter_schedule %>%
    filter(wk > i) %>%
    select(-pts,
           -opp) %>%
    left_join(filter_ratings,
              by = c('school' = 'school')) %>%
    rename(school_elo = rating) %>%
    mutate(school_elo = ifelse(is.na(school_elo), 1500, school_elo)) %>%
    left_join(filter_ratings,
              by = c('opponent' = 'school')) %>%
    rename(opp_elo = rating) %>%
    mutate(opp_elo = ifelse(is.na(opp_elo), 1500, opp_elo)) %>%
    select(-opp_conf) %>%
    mutate(school_elo_adj = case_when(location == '' ~ school_elo + 75,
                                      location == '@' ~ school_elo + (75*-1),
                                      location == 'N' ~ school_elo + 0)) %>%
    mutate(opp_elo_adj = case_when(location == '@' ~ opp_elo + 75,
                                   location == '' ~ opp_elo + (75*-1),
                                   location == 'N' ~ opp_elo + 0)) %>%
    mutate(m = (school_elo_adj - opp_elo_adj)/400,
           elo_diff = abs(school_elo_adj-opp_elo_adj),
           ps = round(elo_diff/25, 1),
           p_opponent = 1/(1+10^m),
           p_school = 1-p_opponent) %>%
    select(-opp_elo_adj,
           -school_elo_adj,
           -m,
           -elo_diff,
           -ps,
           -p_opponent)
  
  rep_df <- replicate(n_times, test, simplify = FALSE)
  
  combine_results <- rbindlist(lapply(rep_df, probability))
  
  road_to_six <- six_win_results %>%
    group_by(school,
             opponent,
             wk) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    mutate(wk_iter = i)
  
  total_wins <- combine_results %>%
    group_by(school,
             conf,
             div,
             total_wins) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    group_by(school,
             conf,
             div) %>%
    mutate(freq_p = freq/sum(freq)) %>%
    ungroup() %>%
    arrange(school,
            total_wins) %>%
    mutate(wk = i)
  
  average_results <- combine_results %>%
    group_by(school,
             conf,
             div) %>%
    summarise(c_wins = max(c_wins),
              c_lose = max(c_lose),
              avg_wins = round(mean(total_wins)),
              avg_loses = round(mean(total_losses)),
              bowl_times = sum(bowl),
              runs_table = sum(runs_table),
              win_out_times = sum(wins_out)) %>%
    ungroup() %>%
    mutate(wk = i) 
  
  six_wins_list[[i]] <- road_to_six
  total_wins_list[[i]] <- total_wins
  week_forecast[[i]] <- average_results
  print(i)
  
}

range_games_won <- rbindlist(total_wins_list)
weekly_forecast <- rbindlist(week_forecast)

