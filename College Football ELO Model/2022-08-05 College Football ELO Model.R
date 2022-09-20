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
final_elo_ratings <- list()
weekly_list <- list()
season_movements <- list()
for(a in season_var){
  
  #a <- 1972
  
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
    
    week_season <- use_season %>%
      filter(wk == i) %>%
      #filter(School == 'Nebraska') %>%
      left_join(use_elo_df,
                by = c('school' = 'school')) %>%
      rename(school_elo = rating) %>%
      left_join(use_elo_df,
                by = c('opponent' = 'school')) %>%
      rename(opp_elo = rating) %>%
      mutate(school_elo_adj = case_when(location == '' ~ school_elo + homeadv,
                                        location == '@' ~ school_elo + awayadv,
                                        location == 'N' ~ school_elo + 0)) %>%
      mutate(opp_elo_adj = case_when(location == '@' ~ opp_elo + homeadv,
                                     location == '' ~ opp_elo + awayadv,
                                     location == 'N' ~ opp_elo + 0)) %>%
      mutate(m = (school_elo_adj - opp_elo_adj)/400,
             elo_diff = abs(school_elo_adj-opp_elo_adj),
             ps = round(elo_diff/25, 1),
             p_opponent = 1/(1+10^m),
             p_team = 1-p_opponent,
             margin_abs = abs(pts-opp),
             mov_mult = log(margin_abs + 1)*(2.2/(elo_diff * 0.001 + 2.2)),
             k_adj = k_val*log(margin_abs + 1),
             elo_adj = (k_val * mov_mult) * (1-p_team),
             team_elo_update = case_when(wins == 1 ~ school_elo + elo_adj,
                                         ties == 1 ~ school_elo,
                                         loses == 1 ~ school_elo + (elo_adj*-1) ),
             opponent_elo_update = case_when(loses == 1 ~ opp_elo + elo_adj,
                                             ties == 1 ~ opp_elo,
                                             wins == 1 ~ opp_elo + (elo_adj * -1) ))
    
    week_update <- week_season %>%
      select(season,
             wk,
             month,
             day,
             year,
             school,
             opponent,
             location,
             pts,
             opp,
             ps,
             elo_adj,
             p_team,
             p_opponent,
             school_elo = team_elo_update,
             opponent_elo = opponent_elo_update)
    
    school_elo <- week_update %>%
      select(school,
             rating = school_elo)
    
    opponent_elo <- week_update %>%
      select(school = opponent,
             rating = opponent_elo)
    
    new_ratings <- bind_rows(use_elo_df, school_elo, opponent_elo) %>%
      group_by(school) %>%
      mutate(index = row_number()) %>%
      filter(index == max(index)) %>%
      ungroup() %>%
      arrange(desc(rating))
    
    if(i == end_week){
      
      #print('Regress happens')
      
      season_conf <- conferences %>%
        filter(season == a) %>%
        #mutate(Conf = trim(Conf)) %>%
        select(school,
               conf)
      
      update_ratings <- new_ratings %>%
        left_join(season_conf,
                  by = c('school' = 'school')) %>%
        group_by(conf) %>%
        mutate(mean_conf_elo = mean(rating)) %>%
        ungroup() %>%
        mutate(regress_rating = case_when(rating > mean_conf_elo ~ rating-((rating-mean_conf_elo)*regress_val),
                                          is.na(conf) ~ 1500,
                                          rating < mean_conf_elo ~ rating + ((mean_conf_elo-rating)*regress_val))) %>%
        arrange(desc(rating))
      
      new_ratings <- update_ratings %>%
        select(school,
               rating = regress_rating) %>%
        arrange(desc(rating)) 
      
      export_ratings <- update_ratings %>%
        mutate(Season = a)
      
      weekly_movements <- new_ratings %>%
        mutate(season = a,
               wk = i)
      
      final_elo_ratings[[a]] <- export_ratings
      
      print(a)
      print(export_ratings)
      
    }else{
      
      #print('No Regress happens')
      
      new_ratings <- new_ratings
      
      weekly_movements <- new_ratings %>%
        mutate(season = a,
               wk = i)
      
      
    }
    
    week_games[[i]] <- week_update
    
  }
  
  season_df <- rbindlist(week_games)
  weekly_df <- rbindlist(weekly_list, fill = TRUE)
  
  season_games[[a]] <- season_df
  season_movements[[a]] <- weekly_df
  
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

write.csv(ELODF, 'C:/Users/alexe/Desktop/ELODF.csv')
write.csv(FullELODF, '~/FullELODF.csv')
