# trend the percent of games won by each college football team since WWII every 5 seasons
# data scrapped from College Football Reference
# dashboard create in Tableau
# code by alex elfering

library(data.table)
library(tidyverse)
library(ggplot2)
library(rvest)
library(stringi)
library(glue)
library(zoo)

# trim function https://stackoverflow.com/questions/2261079/how-can-i-trim-leading-and-trailing-white-space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# load college football results
FullSchedule <- read.csv('C:/Users/alexe/Desktop/FBS Full Schedule.csv') %>% select(-X)
Conferences <- read.csv('C:/Users/alexe/Desktop/Conferences.csv') %>% mutate(Conf = trim(Conf), Div = trim(Div)) %>% select(-X)

# data cleaning and identify wins/losses/ties and other metrics
school_results <- FullSchedule %>%
  mutate(Wins = ifelse(Pts > Opp, 1, 0),
         Loses = ifelse(Pts < Opp, 1, 0),
         Ties = ifelse(Pts == Opp, 1, 0),
         PD = Pts-Opp,
         Close_Game = ifelse((PD <= 7 & PD >= -7) & PD != 0, 1, 0),
         Field_Goal_Game = ifelse((PD <= 3 & PD >= -3) & PD != 0, 1, 0),
         Blow_Out = ifelse(Close_Game == 0, 1, 0),
         Close_Win = ifelse(Close_Game ==1 & Wins == 1, 1, 0),
         Field_Goal_Win = ifelse(Field_Goal_Game ==1 & Wins == 1, 1, 0),
         Blow_Out_Win = ifelse(Close_Game == 0 & Wins == 1, 1, 0),
         Shut_Out = ifelse(Opp == 0, 1, 0),
         Shut_OutA = ifelse(Pts == 0, 1, 0),
         Game = 1) %>%
  filter(Season > 1945) %>%
  unite(Date, c('Month', 'Day', 'Year'), sep = '-') %>%
  inner_join(Conferences,
             by = c('Season' = 'Season',
                    'School' = 'Var.2')) %>%
  rename(Opp_Conf = Conf.x,
         Conf = Conf.y) %>%
  mutate( Conf = trim(Conf),
          Conf_Game = ifelse(Conf == Opp_Conf & (Conf != 'Ind' & Opp_Conf!= 'Ind') , 1, 0),
  ) %>%
  select(-Notes,
         -Div) 

season_stats <- school_results %>%
  select(Season,
         School,
         Conf,
         Game,
         Conf_Game,
         Wins,
         Loses,
         Ties,
         Close_Game,
         Blow_Out,
         Blow_Out_Win,
         Close_Game,
         Close_Win,
         Shut_Out,
         Shut_OutA) %>%
  group_by(Season,
           School,
           Conf) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup() 

# identifies when a program goes on a break
# for instance, making sure the rolling function doesn't count when Appalachian State played in FBS until '81 and resumed in 2014
school_pivot_list <- list()
for(i in unique(season_stats$School)){
  
  school_filter <- season_stats %>%
    filter(School == i) %>%
    mutate(Not_NA = ifelse(!is.na(Conf), 1, 0)) %>%
    mutate(Row = rowid(rleid(Not_NA)) * Not_NA) %>%
    filter(!is.na(Conf)) %>%
    select(-Not_NA) %>%
    select(-Conf_Game) %>%
    pivot_longer(cols = c('Wins', 
                          'Loses',
                          'Ties',
                          'Close_Game',
                          'Blow_Out',
                          'Blow_Out_Win',
                          'Close_Win',
                          'Shut_Out',
                          'Shut_OutA'),
                 names_to = 'Metric',
                 values_to = 'Games') 
  
  school_group <- school_filter %>%
    filter(Row == 1) %>%
    select(School,
           Season,
           Row) %>%
    distinct() %>%
    mutate(Group = row_number()) %>%
    select(School,
           Season,
           Group)
  
  school_filter_group <- school_filter %>%
    left_join(school_group) %>%
    fill(Group) %>%
    group_by(Metric,
             Group) %>%
    mutate(Roll_Total_Games = rollapplyr(Game, 5, sum, partial = TRUE),
           Roll_Games = rollapplyr(Games, 5, sum, partial = TRUE),
           Pct = Roll_Games/Roll_Total_Games) %>%
    ungroup() %>%
    as.data.frame()

  school_pivot_list[[i]] <- school_filter_group
  
}

school_metrics <- rbindlist(school_pivot_list)

write.csv(school_metrics, 'school_metrics.csv')