# CFB Schedule Scrapping
# pulls school wins, losses, and conference data to use in the ELO Model
# HUGE THANKS TO https://www.sports-reference.com/ for providing the data

# data and variable loading  ----
source('~/CFB Parent Variables.R')

# pull cfb schedules  ----

schools_url <- 'https://www.sports-reference.com/cfb/schools/'

schools <- read_html(schools_url)

change_school_names(schools_list, school, school) 

schools_list <- schools %>% 
  html_table(fill = TRUE) %>%
  as.data.frame() %>%
  select(2, 3, 4) %>%
  filter(Var.2 != '',
         Var.2 != 'School') %>%
  mutate(Var.3 = as.numeric(Var.3),
         Var.4 = as.numeric(Var.4)) %>%
  group_by(Var.2) %>%
  complete(Var.3 = seq(min(Var.3), max(Var.4))) %>%
  rename(school = 1,
         seasons = 2) %>%
  select(-Var.4) %>%
  mutate(school = case_when(school == 'UTEP' ~ 'Texas-El Paso',
                            school == 'UAB' ~ 'Alabama-Birmingham',
                            school == 'BYU' ~ "Brigham Young",
                            school == 'UCF' ~ "Central Florida",
                            school == "LSU" ~ "Louisiana State",
                            school == "Ole Miss" ~ "Mississippi",
                           school == "Pitt" ~ "Pittsburgh",
                           school == "USC" ~ 'Southern California',
                           school == 'SMU' ~ "Southern Methodist",
                           school == 'UTSA' ~ 'Texas-San Antonio',
                           TRUE ~ school)) %>%
  filter(school != '') %>%
  mutate(school = tolower(school),
         school = gsub(' ', '-', school),
         school = gsub('\\-&-', '-', school),
         school = gsub('\\&', '', school),
         school = gsub('[()]', '', school)) %>%
  rename(school_code = school) %>%
  mutate(school_url = paste('https://www.sports-reference.com/cfb/schools/',school_code,'/',seasons,'-schedule.html', sep = ''))

# pull schedules for each school  ----
school_seasons_schedule_list <- list()
for(i in schools_list$school_url){
  
  tryCatch({
    schedules <- read_html(i)
    schedules_list <- schedules %>% html_table(fill = TRUE)
    
    if(length(schedules_list) == 1){
      cfb_item <- as.data.frame(schedules_list[1])
    }else if(length(schedules_list) == 2){
      cfb_item <- as.data.frame(schedules_list[2])
    }
    
    print(i)
    print(cfb_item)
    
    school_seasons_schedule_list[[i]] <- cfb_item
    
  }, error=function(e){})
  
}

# pull the weeks for each season  ----
seasons_dates_list <- list()
for(i in season_var){
  
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
    
    seasons_dates_list[[i]] <- seasons_dates
    
  }, error=function(e){})
  
}

# pull the conferences by school for each season  ----
school_conf_list <- list()
for(i in season_var){
  
  tryCatch({
    
    seasons_url <- glue('https://www.sports-reference.com/cfb/years/{i}-ratings.html')
    
    schedules <- read_html(seasons_url)
    ratings_df <- schedules %>% 
      html_table(fill = TRUE) %>%
      as.data.frame() %>%
      select(2, 3)
    
    colnames(ratings_df) <- gsub(" ", "_", as.character(ratings_df[1,]))
    
    school_conf_div <- ratings_df %>%
      slice(-1) %>%
      select(School,
             Conf) %>%
      separate(Conf, into = c('Conf', 'Div'),sep = '\\(') %>%
      mutate(Div = gsub('\\)', '', Div)) %>%
      mutate(Season = i) %>%
      filter(School != '',
             School != 'School')
    
    school_conf_list[[i]] <- school_conf_div
    
  }, error=function(e){})
  
}

conferences <- rbindlist(school_conf_list, fill = TRUE) %>%
  mutate(School = case_when(School == 'UTEP' ~ 'Texas-El Paso',
                            School == 'UAB' ~ 'Alabama-Birmingham',
                            School == 'BYU' ~ "Brigham Young",
                            School == 'UCF' ~ "Central Florida",
                            School == "LSU" ~ "Louisiana State",
                            School == "Ole Miss" ~ "Mississippi",
                            School == "Pitt" ~ "Pittsburgh",
                            School == "USC" ~ 'Southern California',
                            School == 'SMU' ~ "Southern Methodist",
                            School == 'UTSA' ~ 'Texas-San Antonio',
                              TRUE ~ School),
         School = trim(School),
         Conf = trim(Conf))

season_dates <- rbindlist(seasons_dates_list, fill = TRUE)

# clean the schedules ----
# return the season for each date respectively

clean_schedule_df <- rbindlist(school_seasons_schedule_list, fill = TRUE) %>%
  select(-Var.8,
         -Var.9) %>%
  unite(Location, c('Var.5', 'Var.6'), na.rm = TRUE) %>%
  mutate(school_ranking = gsub('[A-Za-z]', '', School),
         opponent_ranking = gsub('[A-Za-z]', '', Opponent),
         school_ranking = gsub('[:punct:]', '', school_ranking),
         opponent_ranking = gsub('[:punct:]', '', opponent_ranking)) %>%
  mutate(School = trim(stri_trim_both(str_remove_all(School, paste(rank_patterns, collapse = "|")))),
         Opponent = trim(stri_trim_both(str_remove_all(Opponent, paste(rank_patterns, collapse = "|"))))) %>%
  mutate(School = case_when(School == 'UTEP' ~ 'Texas-El Paso',
                            School == 'UAB' ~ 'Alabama-Birmingham',
                            School == 'BYU' ~ "Brigham Young",
                            School == 'UCF' ~ "Central Florida",
                            School == "LSU" ~ "Louisiana State",
                            School == "Ole Miss" ~ "Mississippi",
                            School == "Pitt" ~ "Pittsburgh",
                            School == "USC" ~ 'Southern California',
                            School == 'SMU' ~ "Southern Methodist",
                            School == 'UTSA' ~ 'Texas-San Antonio',
                            TRUE ~ School),
         School = trim(School)) %>%
  mutate(Opponent = case_when(Opponent == 'UTEP' ~ 'Texas-El Paso',
                              Opponent == 'UAB' ~ 'Alabama-Birmingham',
                              Opponent == 'BYU' ~ "Brigham Young",
                              Opponent == 'UCF' ~ "Central Florida",
                              Opponent == "LSU" ~ "Louisiana State",
                              Opponent == "Ole Miss" ~ "Mississippi",
                              Opponent == "Pitt" ~ "Pittsburgh",
                              Opponent == "USC" ~ 'Southern California',
                              Opponent == 'SMU' ~ "Southern Methodist",
                              Opponent == 'UTSA' ~ 'Texas-San Antonio',
                              TRUE ~ Opponent),
         Opponent = trim(Opponent))


games_lost <- clean_schedule_df %>%
  select(Date,
         #Day,
         School = Opponent,
         Location,
         Opponent = School,
         Pts = Opp,
         Opp = Pts) %>%
  mutate(Location = case_when(Location == '@' ~ '',
                              Location == '' ~ '@',
                              Location == 'N' ~ 'N'))

games_won <- clean_schedule_df %>%
  select(Date,
         School,
         Location,
         Opponent,
         Pts,
         Opp)

full_cfb_schedule <- bind_rows(games_won, games_lost) %>%
  distinct() %>%
  left_join(season_dates) %>%
  left_join(conferences, 
            by = c('Opponent' = 'School',
                   'Season' = 'Season')) %>%
  select(-Div) %>%
  rename(Opp_Conf = Conf) %>%
  #    ensure that the matches are ordered by year, month, and date
  separate(Date, 
           into = c('Month', 'Day', 'Year'), 
           sep = ' ') %>%
  mutate(Month = match(trim(Month), month.abb),
         Day = as.numeric(gsub('\\,', '', Day),
                          Year = as.numeric(trim(Year)))) %>%
  arrange(Year,
          Month,
          Day)

winning_games <- full_cfb_schedule %>%
  mutate(Wins = ifelse(Pts > Opp, 1, 0),
         HomeAdv = case_when(Location == '' ~ adv,
                             Location == '@' ~ adv*-1,
                             Location == 'N' ~ 0),
         AwayAdv = case_when(Location == '' ~ adv*-1,
                             Location == '@' ~ adv,
                             Location == 'N' ~ 0)) %>%
  filter(Pts > Opp) %>%
  arrange(Year,
          Month,
          Day) %>%
  as.tibble()

colnames(full_cfb_schedule) <- tolower(colnames(full_cfb_schedule))
colnames(winning_games) <- tolower(colnames(winning_games))
colnames(conferences) <- tolower(colnames(conferences))

write.csv(conferences, '~/GitHub/Sports/College Football Schedule Scrapping/Data/Conferences.csv')
write.csv(full_cfb_schedule, '~/GitHub/Sports/College Football Schedule Scrapping/Data/FBS Full Schedule.csv')
write.csv(winning_games, '~/GitHub/Sports/College Football Schedule Scrapping/Data/FBS Winning Games.csv')

