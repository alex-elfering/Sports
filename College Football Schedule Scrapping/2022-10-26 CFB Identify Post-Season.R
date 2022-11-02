# CFB Game Data Scrapper
# scrapes all bowl games and post-season games to exclude in forecasting and visualizationa
# HUGE THANKS TO https://www.sports-reference.com/ for providing the data

library(glue)
library(tidyverse)
library(rvest)
library(data.table)
library(lubridate)
library(stringi)

fbs_schedule <- read.csv('~/GitHub/Sports/College Football Schedule Scrapping/Data/FBS Full Schedule.csv') %>% select(-1)

rank_patterns <- paste('\\(', 1:25, '\\)', sep = '')

post_seasons <- 1946:year(Sys.Date())-1

post_season_list <- list()
for(i in post_seasons){
  
  print(i)
  
  post_season <- read_html(glue('https://www.sports-reference.com/cfb/years/{i}-bowls.html'))
  
  post_data <- post_season %>% 
    html_table(fill = TRUE) %>%
    as.data.frame()
  
  
  post_season_df <- select(post_data, -contains("Pts")) %>% mutate(Season = i)
  
  post_season_list[[i]] <- post_season_df
  
}

seasons_list <- list()
for(i in post_seasons){
  
  tryCatch({
    
    seasons_url <- glue('https://www.sports-reference.com/cfb/years/{i}-schedule.html')
    
    schedules <- read_html(seasons_url)
    schedule_season <- schedules %>% html_table(fill = TRUE)
    
    unique_games <- schedule_season[1] %>%
      as.data.frame() %>%
      mutate(Season = i) %>%
      #select(Season,
      #       Wk,
      #       Date) %>%
      distinct() %>%
      filter(Date != 'Date')
    
    print(unique_games)
    
    seasons_list[[i]] <- unique_games
    
  }, error=function(e){})
  
}

post_seasons_dates_list <- list()
for(i in post_seasons){
  
  tryCatch({
    
    seasons_url <- glue('https://www.sports-reference.com/cfb/years/{i}-schedule.html')
    
    schedules <- read_html(seasons_url)
    schedule_season <- schedules %>% html_table(fill = TRUE)
    
    seasons_dates <- schedule_season[1] %>%
      as.data.frame() %>%
      mutate(Season = i) %>%
      select(Season,
             Wk,
             Date) %>%
      distinct() %>%
      filter(Date != 'Date')
    
    print(seasons_dates)
    
    post_seasons_dates_list[[i]] <- seasons_dates
    
  }, error=function(e){})
  
}

init_post_season <- rbindlist(post_season_list, fill = TRUE) %>%
  unite(Var.3, c(Var.3, Var.4), sep = '', na.rm = TRUE) %>%
  unite(Var.5, c(Var.5, Var.6), sep = '', na.rm = TRUE)

weeks_post <- rbindlist(post_seasons_dates_list) %>% mutate(Date = as.Date(gsub(',', '', Date), "%b %d %Y"))

remove_ranks_post <- init_post_season %>%
  mutate(Var.3 = trim(stri_trim_both(str_remove_all(Var.3, paste(rank_patterns, collapse = "|")))),
         Var.5 = trim(stri_trim_both(str_remove_all(Var.5, paste(rank_patterns, collapse = "|"))))) %>%
  select(Season,
         Date,
         Var.3,
         Var.5) %>%
  mutate(Date = ymd(Date),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date))

post_lost <- remove_ranks_post %>%
  select(Year,
         Month,
         Day,
         Date,
         Season,
         Var.3 = Var.5,
         Var.5 = Var.3)

complete_bowls <- bind_rows(remove_ranks_post, post_lost) %>%
  rename(School = Var.3,
         Opponent = Var.5) 

conf_champ_winner <- rbindlist(seasons_list, fill = TRUE)  %>%
  filter(grepl('championship', Notes, ignore.case = TRUE)) %>%
  mutate(Date = as.Date(gsub(',', '', Date), "%b %d %Y"),
         Month = month(Date),
         Day = day(Date),
         Year = year(Date)) %>%
  rename(School = Winner,
         Opponent = Loser) %>%
  mutate(School = trim(stri_trim_both(str_remove_all(School, paste(rank_patterns, collapse = "|")))),
         Opponent = trim(stri_trim_both(str_remove_all(Opponent, paste(rank_patterns, collapse = "|"))))) %>%
  select(Month,
         Date,
         Day,
         Year,
         Season,
         School,
         Opponent) %>%
  mutate(Year = as.integer(Year))

conf_champ_loser <- conf_champ_winner %>%
  rename(School = Opponent,
         Opponent = School)

conf_post <- bind_rows(conf_champ_winner, conf_champ_loser) %>%
  #select(-Date) %>%
  bind_rows(complete_bowls) %>%
  inner_join(weeks_post)

colnames(conf_post) <- tolower(colnames(conf_post))

write.csv(conf_post, '~/GitHub/Sports/College Football Schedule Scrapping/Data/Post Season and Championships.csv')




