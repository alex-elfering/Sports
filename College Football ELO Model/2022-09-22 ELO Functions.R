# ELO functions

change_school_names <- function(df, column_name, new_name){
  
  column_name = enquo(column_name)
  
  df_new <- df %>%
    mutate(new_name = case_when(!!column_name == 'UTEP' ~ 'Texas-El Paso',
                                !!column_name == 'UAB' ~ 'Alabama-Birmingham',
                                !!column_name == 'BYU' ~ "Brigham Young",
                                !!column_name == 'UCF' ~ "Central Florida",
                                !!column_name == "LSU" ~ "Louisiana State",
                                !!column_name == "Ole Miss" ~ "Mississippi",
                                !!column_name == "Pitt" ~ "Pittsburgh",
                                !!column_name == "USC" ~ 'Southern California',
                                !!column_name == 'SMU' ~ "Southern Methodist",
                                !!column_name == 'UTSA' ~ 'Texas-San Antonio',
                                TRUE ~ !!column_name))
  
  return(df_new)
  
}

elo_week_update <- function(df, week_int = i, team_adv = 75, opp_adv = -75, k_val = 30){
  
  week_season <- use_season %>%
    filter(wk == i) %>%
    left_join(use_elo_df,
              by = c('school' = 'school')) %>%
    rename(school_elo = rating) %>%
    left_join(use_elo_df,
              by = c('opponent' = 'school')) %>%
    rename(opp_elo = rating) %>%
    mutate(school_elo_adj = case_when(location == '' ~ school_elo + team_adv,
                                      location == '@' ~ school_elo + (team_adv*-1),
                                      location == 'N' ~ school_elo + 0)) %>%
    mutate(opp_elo_adj = case_when(location == '@' ~ opp_elo + team_adv,
                                   location == '' ~ opp_elo + (team_adv*-1),
                                   location == 'N' ~ opp_elo + 0)) %>%
    mutate(m = (school_elo_adj - opp_elo_adj)/400,
           elo_diff = abs(school_elo_adj-opp_elo_adj),
           ps = round(elo_diff/25, 1),
           p_opponent = 1/(1+10^m),
           p_team = 1-p_opponent,
           margin_abs = abs(pts-opp),
           mov_mult = log(margin_abs + 1)*(2.2/(elo_diff * 0.001 + 2.2)),
           #k_adj = k_val*log(margin_abs + 1),
           elo_adj = (k_val * mov_mult) * (1-p_team),
           team_elo_update = case_when(wins == 1 ~ school_elo + elo_adj,
                                       ties == 1 ~ school_elo,
                                       loses == 1 ~ school_elo + (elo_adj*-1) ),
           opponent_elo_update = case_when(loses == 1 ~ opp_elo + elo_adj,
                                           ties == 1 ~ opp_elo,
                                           wins == 1 ~ opp_elo + (elo_adj * -1) )) %>%
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
  
}

team_elo_scores <- function(df){
  
  school_elo <- df %>%
    select(school,
           rating = school_elo)
  
  opponent_elo <- df %>%
    select(school = opponent,
           rating = opponent_elo)
  
  new_ratings <- bind_rows(use_elo_df, school_elo, opponent_elo) %>%
    group_by(school) %>%
    mutate(index = row_number()) %>%
    filter(index == max(index)) %>%
    ungroup() %>%
    arrange(desc(rating)) 
  
  #print(new_ratings)
  
}

regress_ratings <- function(conf_df, df, regress_val = 0.3){
  
  update_ratings <- df %>%
    left_join(conf_df,
              by = c('school' = 'school')) %>%
    group_by(conf) %>%
    mutate(mean_conf_elo = mean(rating)) %>%
    ungroup() %>%
    mutate(fbs = ifelse(!is.na(conf), 1, 0)) %>%
    group_by(fbs) %>%
    mutate(overall_mean = mean(rating)) %>%
    ungroup() %>%
    mutate(fbs_team = ifelse(!is.na(conf), 1, 0)) %>%
    mutate(regress_rating = case_when(rating > mean_conf_elo & conf != 'Ind' ~ rating-((rating-mean_conf_elo)*regress_val),
                                      rating < mean_conf_elo & conf != 'Ind' ~ rating + ((mean_conf_elo-rating)*regress_val),
                                      is.na(conf) ~ 1500,
                                      rating > overall_mean & conf == 'Ind' ~ rating-((rating-overall_mean)*regress_val),
                                      rating < overall_mean & conf == 'Ind' ~ rating + ((overall_mean-rating)*regress_val))) %>%
    select(school,
           rating,
           regress_rating,
           mean_conf_elo,
           overall_mean) %>%
    arrange(desc(rating))
  
 # print(update_ratings)
  
  
}