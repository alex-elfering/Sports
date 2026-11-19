# college basketball data cleaning script
# takes data compiled by college-basketball reference and cleans it for data visualization uses

library(tidyverse)
library(janitor)
library(glue)
library(lubridate)

documents_loc <- "~"

# nicknames vector  ----
school_short_names <- c(
  "Brigham Young"                   = "BYU",
  "Southern California"             = "USC",
  "Southern Methodist"              = "SMU",
  "Texas Christian"                 = "TCU",
  "Louisiana State"                 = "LSU",
  "Virginia Commonwealth"           = "VCU",
  "Virginia Military Institute"     = "VMI",
  "Nevada–Las Vegas"                = "UNLV",
  "Connecticut"                     = "UConn",
  "Massachusetts"                   = "UMass",
  "Massachusetts–Lowell"            = "UMass Lowell",
  "Pennsylvania"                    = "Penn",
  "Pittsburgh"                      = "Pitt",
  "Mississippi"                     = "Ole Miss",
  "Southern Mississippi"            = "Southern Miss",
  "California"                      = "Cal",
  "George Washington"               = "GW",
  "William & Mary"                  = "W&M",
  "Miami (FL)"                      = "Miami",
  "St. John's (NY)"                 = "St. John's",
  "Albany (NY)"                     = "Albany",
  "Loyola (IL)"                     = "Loyola Chicago",
  "Loyola (MD)"                     = "Loyola Maryland",
  "Loyola Marymount"                = "LMU",
  "Saint Francis (PA)"              = "Saint Francis",
  "Illinois–Chicago"                = "UIC",
  "Marylandâ€“Baltimore County"     = "UMBC",
  "Maryland–Baltimore County"       = "UMBC",
  "Maryland–Eastern Shore"          = "UMES",
  "Southern Illinois–Edwardsville"  = "SIUE",
  "Tennessee–Martin"                = "UT Martin",
  "Louisiana–Monroe"                = "UL Monroe",
  "Arkansas–Pine Bluff"             = "Arkansas-Pine Bluff",
  "Texas A&M–Corpus Christi"        = "A&M-Corpus Christi",
  "Texas–Rio Grande Valley"         = "UTRGV",
  "Florida International"           = "FIU",
  "Florida Atlantic"                = "FAU",
  "Florida Gulf Coast"              = "FGCU",
  "Long Island University"          = "LIU",
  "Stephen F. Austin"               = "SFA",
  "Southeast Missouri State"        = "SEMO",
  "Southeastern Louisiana"          = "SE Louisiana",
  "Central Connecticut State"       = "Central Conn.",
  "Northern Kentucky"               = "NKU",
  "Western Kentucky"                = "WKU",
  "Eastern Kentucky"                = "EKU",
  "North Carolina A&T"              = "NC A&T",
  "North Carolina Central"          = "NC Central",
  "Mississippi Valley State"        = "Miss. Valley St.",
  "Prairie View A&M"                = "Prairie View",
  "Cal State Bakersfield"           = "CSU Bakersfield",
  "Cal State Fullerton"             = "CSU Fullerton",
  "Cal State Los Angeles"           = "CSU LA",
  "Cal State Northridge"            = "CSUN",
  "Bethune-Cookman"                 = 'B-CU'
)

# data loading  -----------------------------------
mcbb_games <- read.csv(file.path(documents_loc, 'schedules_2012_2026.csv')) |>
  clean_names() |>
  mutate(
    game_won = ifelse(tm>opp,1,0),
    game_lost = ifelse(tm<opp,1,0)
  )

# script provides clean slug names to add school name for provided slugs
source('~/20261105 School Slug Cleaning.R')

# data cleaning -----------------------------------
clean_mcbb <- mcbb_games |>
  unite(curr_record,c('w','l'),sep='-',na.rm = T) |>
  mutate(date = mdy(date)) |>
  inner_join(
    mcbb_slugnames, 
    by = c('slug' = 'school_slug')
  ) |>
  select(
    season,
    school_name = school_short,
    slug,
    g,
    date,
    type,
    opponent,
    loc = x,
    opp_conference = conf,
    result = x_1,
    tm,
    opp,
    ot,
    curr_record,
    game_won,
    game_lost
  ) |>
  mutate(opponent = str_squish(opponent)) |>   # <- trims ends and collapses internal runs of whitespace
  extract(
    opponent,
    into = c("opponent_name", "opponent_rank"),
    regex = "^(.*?)\\s*(?:\\((\\d+)\\))?$",
    remove = TRUE,    # drop the original so no untrimmed copy sticks around
    convert = TRUE
  ) |>
  mutate(opponent_name = trimws(opponent_name)) |>
  group_by(
    season,
    slug
  ) |>
  mutate(
    ncaa_appearance = max(ifelse(grepl('NCAA',type),1,0)),
    cit_appearance = max(ifelse(grepl('CIT',type),1,0)),
    cbi_appearance = max(ifelse(grepl('CBI',type),1,0)),
    nit_appearance = max(ifelse(grepl('NIT',type),1,0)),
    cbc_appearance = max(ifelse(grepl('CBC',type),1,0)),
  ) |>
  ungroup() 

school_conferences <- clean_mcbb |>
  distinct(
    season, opponent_name, opp_conference
  ) |>
  rename(school = opponent_name, conference = opp_conference)

tournament_winner_list <- clean_mcbb |>
  left_join(school_conferences,
            by = c('school_name' = 'school',
                   'season' = 'season')) |>
  filter(
    type == 'CTOURN'
  ) |>
  filter(result == 'W') |>
  group_by(
    season,
    conference
  ) |>
  slice_max(date) |>
  ungroup() |>
  select(
    season,
    school_name,
    conference
  ) |>
  distinct() |>
  mutate(tourney_winner = 1)

regular_conf_champions <- clean_mcbb |>
  left_join(school_conferences,
            by = c('school_name' = 'school',
                   'season' = 'season')) |>
  filter(
    type == 'REG',
    conference != 'Ind',
    conference == opp_conference
  ) |>
  group_by(
    season,
    school_name,
    conference
  ) |>
  summarise(conf_w = sum(game_won,na.rm = T),
            conf_l = sum(game_lost,na.rm = T),
            .groups = 'drop') |>
  group_by(
    season,
    conference
  ) |>
  mutate(pct_conf = conf_w/(conf_w+conf_l)) |>
  filter(pct_conf == max(pct_conf)) |>
  ungroup() |>
  select(
    -conf_w,
    -conf_l,
    -pct_conf
  ) |>
  mutate(
    reg_season_champ = 1
  )

full_cbb_dataset <- clean_mcbb |>
  arrange(season,
          school_name) |>
  left_join(school_conferences,
            by = c('school_name' = 'school',
                   'season' = 'season')) |>
  left_join(tournament_winner_list,
            by = c('season' = 'season',
                   'conference' = 'conference',
                   'school_name' = 'school_name')) |>
  left_join(regular_conf_champions,
            by = c('season' = 'season',
                   'conference' = 'conference',
                   'school_name' = 'school_name'))

saveRDS(full_cbb_dataset, 'full_cbb_dataset_mens.rds')
