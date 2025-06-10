#### ELO MODEL 
# generates historical ELO ratings for each CFB team in FBS
# HUGE THANKS TO https://www.sports-reference.com/ for providing the data

library(here)
# pull in functions ----
source(here("20250603 CFB ELO Functions.R"))

# data sources ----
all_schools <- winning_games$school
all_opponents <- winning_games$opponent

init_ratings <- tibble(school = unique(c(all_opponents, all_schools)),
                       rating = 1500)
use_elo_df <- data.frame()
new_ratings <- data.frame()

season_vector <- c(1872:2024)

week_games <- list()
season_games <- list()
season_regress_ratings <- list()
#weekly_elo_tracking <- list()  # <-- at the top before loop starts
for(a in season_vector){
  
  #a <- 2024
  
  use_season <- winning_games |>
    mutate(wk = as.numeric(wk),
           wins = ifelse(pts > opp, 1, 0),
           loses = ifelse(pts < opp, 1, 0),
           ties = ifelse(pts == opp, 1, 0)) |>
    filter(season == a)
  
  beg_week <- min(use_season$wk)
  end_week <- max(use_season$wk)
  weekly_elo_tracking <- list()  # <-- at the top before loop starts
  season_week_end_regress <- list()
  for(i in beg_week:end_week){
    #i <- 1
    if(i == 1 & a == min(season_vector)){
      use_elo_df <- init_ratings
    }else{
      use_elo_df <- new_ratings
    }
    
    week_update <- elo_week_update(
      df = use_season,
      use_elo_df = use_elo_df,
      week_int = i,
      team_adv = 50,
      k_val = 40
    )
    
    new_ratings <- team_elo_scores(df = week_update, use_elo_df = use_elo_df)
    
    weekly_elo_tracking[[paste0(a, "_", i)]] <- new_ratings |>
      mutate(season = a,
             wk = i)
    
    # regress elo ratings if last week of the season
    if(i == end_week){
      
      season_conf <- conf_df |>
        filter(season == a) |>
        select(school,
               conf)
      
      season_regress <- regress_ratings(conf_df = season_conf, df = new_ratings, regress_val = 0.4)
      
      new_ratings <- season_regress |>
        select(school,
               rating = regress_rating)
      
      print(season_regress |> mutate(season = a))
      #season_week_end_regress[[i]] <- season_regress |> mutate(season = a) |> filter(school == 'Rutgers')
    }else{
      
      new_ratings <- new_ratings
      
      
    }
    #print(season_regress |> mutate(season = a) |> filter(school == 'Rutgers'))
    week_games[[i]] <- week_update
    #season_week_end_regress[[i]] <- season_regress |> mutate(season = a)
    
  }
  
  regress_df <- rbindlist(season_week_end_regress, fill = T)
  season_df <- rbindlist(week_games)
  
  weekly_elo_df <- bind_rows(weekly_elo_tracking) |>
    arrange(season, wk, desc(rating))
  
  season_games[[a]] <- season_df
  season_regress_ratings[[a]] <- regress_df

  #print(rbindlist(season_week_end_regress) |> filter(school == 'Rutgers'))
  
}

elo_df <- rbindlist(season_games, fill = TRUE) |>
  distinct() |>
  rename(elo_a = school_elo,
         elo_b = opponent_elo,
         p_a = p_team,
         team_a = school,
         team_b = opponent) |>
  select(-p_opponent) |>
  arrange(season,
          wk)

full_elo_df <- elo_df |>
  distinct() |>
  select(season,
         wk,
         team_a = team_b,
         team_b = team_a,
         location,
         pts = opp,
         opp = pts,
         ps,
         elo_adj,
         p_a,
         elo_a = elo_b,
         elo_b = elo_a) |>
  mutate(p_a = 1-p_a,
         location = case_when(location == '' ~ '@',
                              location == '@' ~ '',
                              location == 'N' ~ 'N')) |>
  bind_rows(elo_df) |>
  mutate(result = case_when(pts > opp ~ 'W',
                            pts < opp ~ 'L',
                            pts == opp ~ 'T'))

write.csv(full_regress_scores, "C:/Users/alexe/OneDrive/Documents/Sports Analysis/CFB ELO Model/full_regress_scores.csv")
write.csv(elo_df, "C:/Users/alexe/OneDrive/Documents/Sports Analysis/CFB ELO Model/unique_elo_df.csv")
write.csv(full_elo_df, "C:/Users/alexe/OneDrive/Documents/Sports Analysis/CFB ELO Model/full_elo_df.csv")