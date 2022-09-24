#### ELO MODEL 
# generates historical ELO ratings for each CFB team in FBS
# HUGE THANKS TO https://www.sports-reference.com/ for providing the data

library(elo)
library(data.table)
library(tidyverse)
library(ggplot2)
library(rvest)
library(stringi)
library(glue)

# data sources ----
source('~/CFB Parent Variables.R')

all_schools <- winning_games$school
all_opponents <- winning_games$opponent

init_ratings <- tibble(school = unique(c(all_opponents, all_schools)),
                       rating = 1500)
use_elo_df <- data.frame()
new_ratings <- data.frame()

week_games <- list()
season_games <- list()
for(a in season_var){
  
  use_season <- winning_games %>%
    mutate(wk = as.numeric(wk),
           loses = ifelse(pts < opp, 1, 0),
           ties = ifelse(pts == opp, 1, 0)) %>%
    filter(season == a)
  
  beg_week <- min(use_season$wk)
  end_week <- max(use_season$wk)
  
  for(i in beg_week:end_week){
    
    #i <- 1
    
    if(i == 1 & a == 1869){
      use_elo_df <- init_ratings
    }else{
      use_elo_df <- new_ratings
    }
    
    week_update <- elo_week_update(df = use_season, team_adv = 75, opp_adv = -75, k_val = 30)
    
    new_ratings <- team_elo_scores(df = week_update)
    
    if(i == end_week){
      
      #print('Regress happens')
      
      season_conf <- conferences %>%
        filter(season == a) %>%
        #mutate(Conf = trim(Conf)) %>%
        select(school,
               conf)
      
      season_regress <- regress_ratings(conf_df = season_conf, df = new_ratings, regress_val = 0.3)
      
      new_ratings <- season_regress %>%
        select(school,
               rating = regress_rating)
      
      print(a)
      print(season_regress %>% mutate(season = a))
      
    }else{
      
      new_ratings <- new_ratings
      
      
    }
    
    week_games[[i]] <- week_update
    
  }
  
  season_df <- rbindlist(week_games)
  
  season_games[[a]] <- season_df

}

elo_df <- rbindlist(season_games, fill = TRUE) %>%
  distinct() %>%
  rename(elo_a = school_elo,
         elo_b = opponent_elo,
         p_a = p_team,
         team_a = school,
         team_b = opponent) %>%
  select(-p_opponent) %>%
  arrange(year,
          month,
          day)

full_elo_df <- elo_df %>%
  distinct() %>%
  select(season,
         wk,
         month,
         day,
         year,
         team_a = team_b,
         team_b = team_a,
         location,
         pts = opp,
         opp = pts,
         ps,
         elo_adj,
         p_a,
         elo_a = elo_b,
         elo_b = elo_a) %>%
  mutate(p_a = 1-p_a,
         location = case_when(location == '' ~ '@',
                              location == '@' ~ '',
                              location == 'N' ~ 'N')) %>%
  bind_rows(elo_df) 

write.csv(elo_df, '~/GitHub/Sports/College Football ELO Model/Data/ELODF1.csv')
write.csv(full_elo_df, '~/GitHub/Sports/College Football ELO Model/Data/FullELODF1.csv')