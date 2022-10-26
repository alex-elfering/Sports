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

init_post_season <- rbindlist(post_season_list, fill = TRUE) %>%
  unite(Var.3, c(Var.3, Var.4), sep = '', na.rm = TRUE) %>%
  unite(Var.5, c(Var.5, Var.6), sep = '', na.rm = TRUE)

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
         Season,
         Var.3 = Var.5,
         Var.5 = Var.3)

complete_bowls <- bind_rows(remove_ranks_post, post_lost) %>%
  rename(School = Var.3,
         Opponent = Var.5) 

conf_champ <- fbs_schedule %>%
  filter(grepl('championship', Notes, ignore.case = TRUE)) %>%
  select(Month,
         Day,
         Year,
         Season,
         School,
         Opponent) %>%
  mutate(Year = as.integer(Year)) %>%
  bind_rows(CompleteBowls) %>%
  mutate(Month = (trim(Month)),
         Day = (trim(Day)),
         Year = trim(Year),
         Season = (trim(Season)),
         School = trim(School),
         Opponent = trim(Opponent)) 

write.csv(conf_champ, 'C:/Users/alexe/Desktop/Bowl Games.csv')




