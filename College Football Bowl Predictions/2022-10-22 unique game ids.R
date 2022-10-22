# creates a unique id for each game to run in the game forecast

library(tidyverse)
library(stringi)
library(data.table)
library(rvest)
library(glue)

season_var <- c(1869:1870, 1872:2022)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
rank_patterns <- paste('\\(', 1:25, '\\)', sep = '')

seasons_list <- list()
for(i in season_var){
  
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

unique_id_df <- rbindlist(seasons_list, fill = TRUE) %>%
  unite(Location, c('Var.7', 'Var.8'), na.rm = TRUE) %>%
  mutate(Winner = trim(stri_trim_both(str_remove_all(Winner, paste(rank_patterns, collapse = "|")))),
         Loser = trim(stri_trim_both(str_remove_all(Loser, paste(rank_patterns, collapse = "|"))))) %>%
  mutate(unique_id = paste0(Season, Wk, Winner, Loser)) %>%
  mutate(unique_id = gsub(' ', '', unique_id))

list_unique <- unique_id_df$unique_id