# functions for college football game predictions
probability <- function(df){
  
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
    left_join(current_week,
               by = c('school' = 'school')) %>%
    mutate(total_wins = wins + c_wins,
           total_losses = losses + c_lose,
           bowl = ifelse(total_wins/(total_wins + total_losses) >= 0.5, 1, 0),
           runs_table = ifelse(wins/(wins + losses) == 1, 1, 0),
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
