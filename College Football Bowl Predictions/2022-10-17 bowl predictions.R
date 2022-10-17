# college football bowl predictions - which teams are likely to go at least 0.500?

library(tidyverse)
library(data.table)
library(rvest)
library(glue)
library(stringi)

fbs_schedule <- read.csv('~/GitHub/Sports/College Football Schedule Scrapping/Data/FBS Full Schedule.csv') %>% select(-1)
full_elo_df <- read.csv('~/GitHub/Sports/College Football ELO Model/Data/FullELODF.csv') %>% select(-1)
conferences <- read.csv('~/GitHub/Sports/College Football Schedule Scrapping/Data/Conferences.csv') %>% select(-1)

season_vari <- 2022
n_times <- 10000
season_var <- c(1869:1870, 1872:2022)


# unique game ids ----
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
rank_patterns <- paste('\\(', 1:25, '\\)', sep = '')

seasons_list <- list()
for(i in 2022){
  
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
  complete(wk = seq.int(1, 8, 1)) %>%
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
  complete(wk = seq(1, 8)) %>%
  replace(is.na(.), 0) %>%
  mutate(roll_wins = cumsum(wins),
         roll_loses = cumsum(loses)) %>%
  ungroup() %>%
  select(school,
         wk,
         c_wins = roll_wins,
         c_lose = roll_loses)

# functions ----
probability <- function(df){
  
  df_new <- df %>%
    mutate(val = runif(nrow(.), 0, 1),
           wins = ifelse(val <= p_school, 1, 0))
  
  df_select <- df_new %>%
    select(month,
           day,
           year,
           season,
           wk,
           school,
           opponent,
           wins)
  
  df_reverse <- df_select %>%
    rename(opponent = school,
           school = opponent) %>%
    mutate(wins = ifelse(wins == 1, 0, 1))
  
  df_results_bind <- bind_rows(df_select, df_reverse) %>%
    arrange(school,
            wk) %>%
    group_by(school) %>%
    summarise(wins = sum(wins),
              games = n()) %>%
    ungroup() %>%
    mutate(losses = games-wins) %>%
    select(-games) %>%
    inner_join(conferences %>% filter(season == 2022),
               by = c('school' = 'school')) %>%
    inner_join(current_week,
               by = c('school' = 'school')) %>%
    mutate(total_wins = wins + c_wins,
           total_losses = losses + c_lose,
           bowl = ifelse(total_wins/(total_wins + total_losses) >= 0.5, 1, 0),
           wins_out = ifelse(total_wins/(total_wins + total_losses) == 1, 1, 0) )
  
  #print(df_results_bind)
  
} 
sixth_win <- function(df){
  
  df_new <- test %>%
    mutate(val = runif(nrow(.), 0, 1),
           wins = ifelse(val <= p_school, 1, 0))
  
  df_select <- df_new %>%
    select(month,
           day,
           year,
           season,
           wk,
           school,
           opponent,
           wins)
  
  df_reverse <- df_select %>%
    rename(opponent = school,
           school = opponent) %>%
    mutate(wins = ifelse(wins == 1, 0, 1))
  
  bind_rows(df_select, df_reverse) %>%
    arrange(school,
            wk) %>%
    #filter(school == 'Air Force') %>%
    inner_join(current_week %>% filter(c_wins < 6),
               by = c('school' = 'school')) %>%
    group_by(school) %>%
    mutate(roll_wins = cumsum(wins),
           total_wins = roll_wins + c_wins) %>%
    filter(total_wins == 6) %>%
    slice(1) %>%
    select(school,
           opponent,
           wk)
  
} 
full_games <- function(df){
  df_new <- test %>%
    mutate(val = runif(nrow(.), 0, 1),
           wins = ifelse(val <= p_school, 1, 0))
  
  df_select <- df_new %>%
    select(month,
           day,
           year,
           season,
           wk,
           school,
           opponent,
           wins)
  
  df_reverse <- df_select %>%
    rename(opponent = school,
           school = opponent) %>%
    mutate(wins = ifelse(wins == 1, 0, 1))
  
  full_df <- bind_rows(df_select, df_reverse) 
  
  
  
  
  
}
# the forecast  ----
week_forecast <- list()
total_wins_list <- list()
six_wins_list <- list()
for(i in 1:1){
  
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
  six_win_results <- rbindlist(lapply(rep_df, sixth_win))
  
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
    summarise(avg_wins = round(mean(total_wins)),
              avg_loses = round(mean(total_losses)),
              bowl_times = sum(bowl),
              win_out_times = sum(wins_out)) %>%
    ungroup() %>%
    mutate(wk = i) 
  
  six_wins_list[[i]] <- road_to_six
  total_wins_list[[i]] <- total_wins
  week_forecast[[i]] <- average_results
  print(i)
  
}

rbindlist(week_forecast) %>%
  arrange(desc(win_out_times))
