# ELO Testing script

library(glue)

k_vals <- seq(20,60,10)
home_adv_vals <- seq(50,150,10)
regress_vals <- seq(0.2,0.6,0.1)

test_elo_variables <- expand.grid(k_vals, home_adv_vals, regress_vals) %>% 
  mutate(test_no = row_number()) %>%
  rename(k_vals = Var1,
         home_adv_vals = Var2,
         regress_vals = Var3) 

for(l in 1:max(test_elo_variables$test_no)){
  
  #print(l)
  
  filter_test <- dplyr::filter(test_elo_variables, test_no == l)

  # test values
  k_test <- filter_test$k_vals
  home_test <- filter_test$home_adv_vals
  regress_test <- filter_test$regress_vals
  
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
      
      week_update <- elo_week_update(df = use_season, team_adv = home_test, opp_adv = home_test*-1, k_val = k_test)
      
      new_ratings <- team_elo_scores(df = week_update)
      
      if(i == end_week){
        
        #print('Regress happens')
        
        season_conf <- conferences %>%
          filter(season == a) %>%
          #mutate(Conf = trim(Conf)) %>%
          select(school,
                 conf)
        
        season_regress <- regress_ratings(conf_df = season_conf, df = new_ratings, regress_val = regress_test)
        
        new_ratings <- season_regress %>%
          select(school,
                 rating = regress_rating)
        
        #print(a)
        #print(season_regress %>% mutate(season = a))
        
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
  
  game_bins <- full_elo_df %>%
    filter(season >= 2015 & season <= 2021, 
           season != 2020) %>%
    mutate(group = cut(p_a, 
                       breaks = seq(0, 1, 0.05),
                       include.lowest = FALSE),
           wins = ifelse(pts > opp, 1, 0),
           game = 1,
           brier = (p_a-wins)^2 ) %>%
    mutate(test_no = l,
           k_val = k_test,
           home_adv = home_test,
           regress_val = regress_test)
    
  print(l)
  print(filter_test)
  
  #write.csv(elo_df, '~/GitHub/Sports/College Football ELO Model/Data/ELODF1.csv')
  write.csv(game_bins, glue('~/GitHub/Sports/College Football ELO Model/Test Data/ELO Test {l}.csv'))
  
}