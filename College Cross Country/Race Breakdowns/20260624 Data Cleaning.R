# data cleaning step

library(data.table)
library(tidyverse)   
library(janitor)

# ----  paths ----
data_dir <- "~/Midwest Regionals/Files"
out_dir  <- "~/GitHub/Sports/College Cross Country/Race Breakdowns"

# ----  pulling in the data ----
csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
stopifnot("No CSVs found in data_dir" = length(csv_files) > 0)

# read everything as character so column types can't clash between files;
# parsing to numeric happens explicitly below
cross_country_data <- rbindlist(
  setNames(lapply(csv_files, fread, colClasses = "character"), basename(csv_files)),
  fill = TRUE,
  idcol = "source_file"          # makes it easy to trace a bad row back to its file
) |>
  as_tibble()

# ----  functions ----

assign_spotlight <- function(df, spotlight_athlete, spotlight_school){
  df |>
    mutate(color_spotlight = factor(
      case_when(athlete == spotlight_athlete ~ 'spotlight',
                school == spotlight_school ~ 'teammates',
                TRUE ~ "other"),
      levels = c('spotlight','teammates','other'))
    )
}

format_times <- function(time_decimal){
  out <- rep(NA_character_, length(time_decimal))
  ok  <- is.finite(time_decimal)
  total_seconds <- round(time_decimal[ok] * 60, 1)
  out[ok] <- sprintf('%d:%04.1f', as.integer(total_seconds %/% 60), total_seconds %% 60)
  out
}

get_ordinal_suffix <- function(n) {
  suffix <- case_when(
    is.na(n)           ~ NA_character_,
    (n %% 100) %in% 11:13 ~ "th",
    n %% 10 == 1       ~ "st",
    n %% 10 == 2       ~ "nd",
    n %% 10 == 3       ~ "rd",
    TRUE               ~ "th"
  )
  if_else(is.na(n), NA_character_, paste0(n, suffix))
}

# ----  cleaning ----
init_xc_data <- cross_country_data |>
  clean_names() |>
  select(source_file,
         year,
         meet,
         division,
         length,
         athlete,
         school,
         class,
         distance,
         place = pl,
         time,
         pts) |>
  # drop DNF/DNS/etc. and rows with no usable time
  filter(!is.na(time), time != "", !str_detect(time, "[a-zA-Z]")) |>
  # exact duplicates (e.g. the same results file saved twice)
  distinct(across(-source_file), .keep_all = TRUE) |>
  mutate(
    time_new = str_pad(time, width = 7, side = 'left', pad = '0')
  ) |>
  separate(time_new, into = c("mins", "secs"), sep = ":", convert = TRUE, remove = FALSE) |>
  mutate(
    time_decimal = mins + (secs / 60),
    time_decimal_seconds = time_decimal * 60
  ) |>
  select(-mins, -secs) |>
  mutate(
    distance_num  = readr::parse_number(distance),
    distance_unit = case_when(
      str_detect(distance, regex("mile",           ignore_case = TRUE)) ~ "mile",
      str_detect(distance, regex("\\d\\s*k(m)?\\b", ignore_case = TRUE)) ~ "km",
      str_detect(distance, regex("\\d\\s*m(eters?)?\\b", ignore_case = TRUE)) ~ "meter"
    ),
    distance_m = case_when(
      distance_unit == "mile"  ~ distance_num * 1609.344,
      distance_unit == "km"    ~ distance_num * 1000,
      distance_unit == "meter" ~ distance_num
    ),
    converted_from_miles = distance_unit == "mile"
  ) |>
  mutate(#distance_m = readr::parse_number(distance),
         place = readr::parse_number(place)) |>
  arrange(year,
          meet,
          division,
          length, 
          athlete, 
          school, 
          distance_m) |>
  group_by(year,
           meet,
           division,
           length, athlete, school) |>
  mutate(
    segment_km = (distance_m - lag(distance_m, default = 0)) / 1000,
    segment_time_min = time_decimal - lag(time_decimal, default = 0),
    pace_km_min = segment_time_min / segment_km,
    pace_mi_min = pace_km_min * 1.60934,
    pace_km_formatted = format_times(pace_km_min),
    pace_mi_formatted = format_times(pace_mi_min),
    school_finish = dense_rank(place)
  ) |>
  ungroup()

# ----  sanity checks ----
# should be one row per athlete per split; anything left here is conflicting data
dupes <- get_dupes(init_xc_data, year, meet, division, length, athlete, school, distance_m)
if (nrow(dupes) > 0) {
  warning(nrow(dupes), " rows share event/athlete/school/distance_m - check `dupes`")
}

# ----  scoring ----

# teams are eligible to score if they have at least five runners by the final split. Unattached runners are also removed
eligible_teams <- init_xc_data |>
  group_by(year,
           meet,
           division,
           length) |>
  filter(distance_m == max(distance_m, na.rm = TRUE)) |>
  group_by(year,
           meet,
           division,
           length, 
           school) |>
  summarise(n_finishers = n_distinct(athlete), .groups = "drop") |>
  filter(n_finishers >= 5) |>
  filter(
    school != 'Unattached' | !grepl('UNA-',school) 
  ) |>
  select(year,
         meet,
         division,
         length, school)

# calculating points
pts_calc <- init_xc_data |>
  semi_join(eligible_teams, by = c("year","meet","division","length", "school")) |>
  arrange(year,
          meet,
          division,
          length, distance_m, place) |>
  group_by(year,
           meet,
           division,
           length, distance_m, school) |>
  mutate(team_rank = row_number()) |>      
  ungroup() |>
  filter(team_rank <= 7) |>                
  group_by(year,
           meet,
           division,
           length, distance_m) |>
  mutate(pts_new = row_number()) |>        
  ungroup() |>
  transmute(year,
            meet,
            division,
            length, athlete, school, distance_m,
            team_rank, pts_new,
            scorer = team_rank <= 5)

init_xc_data <- init_xc_data |>
  left_join(pts_calc,
            by = c("year","meet","division","length", "athlete", "school", "distance_m"),
            relationship = "many-to-one") |>   # errors if the join would duplicate rows
  mutate(
    pts_logged = abs(readr::parse_number(pts)),        # signs look like noise
    pts_final  = coalesce(pts_logged, as.numeric(pts_new))
  )

# ----  team-level dataframe  ----

team_level_xc_stats <- init_xc_data |>
  #filter(
  #  school == 'Michigan State',
  #  year == 2025,
  #  meet == 'Big Ten Championship',
  #  division == 'Men',
  #  length == '8k'
  #) |>
  select(
    year, meet, division, length, school,
    distance_m,
    athlete, time_decimal,pts_final, place, team_rank
  ) |>
  #group_by(
  #  year,
  #  meet,
  #  division,
  #  length
  #) |>
  #filter(
  #  distance_m == max(distance_m)
  #) |>
  #ungroup() |>
  group_by(
    year,
    meet,
    division,
    length,
    school,
    distance_m
  ) |>
  summarise(
    median_time_overall = median(time_decimal,na.rm = T),
    median_time_scorers = median(time_decimal[team_rank<=5],na.rm = T),
    finish_time_first_runner = median(time_decimal[team_rank==1],na.rm = T),
    finish_time_fifth_runner = median(time_decimal[team_rank==5],na.rm = T),
    finish_time_displacers = median(time_decimal[team_rank %in% 6:7],na.rm = T),
    total_points_scored = sum(pts_final[team_rank<=5],na.rm = T),
    #first_place_runner = max(athlete[team_rank==1],na.rm = T),
    .groups = 'drop'
  ) |>
  mutate(
    scorer_time_spread = (finish_time_fifth_runner-finish_time_first_runner)*60,
    displacer_time_spread = (finish_time_displacers-finish_time_fifth_runner)*60
  ) |>
  group_by(
    year,
    meet,
    division,
    length,
    distance_m
  ) |>
  mutate(
    overall_team_rank_points = dense_rank(total_points_scored)
  )


# ----  saving ----
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(init_xc_data, file.path(out_dir, "clean_xc_data.rds"))
saveRDS(team_level_xc_stats,file.path(out_dir,'team_level_xc_data.rds'))