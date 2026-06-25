# data cleaning step

library(data.table)
library(tidyverse)
library(janitor)
library(readxl)
library(stringr)
library(glue)

# ----  pulling in the data ----
setwd("~/Midwest Regionals/Files")
cross_country_data <- rbindlist(lapply(list.files(pattern = "*.csv"), fread))

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
  sprintf('%d:%04.1f',floor(time_decimal),(time_decimal %% 1)*60)
}
get_ordinal_suffix <- function(n) {
  if (n %% 100 %in% 11:13) return(paste0(n, "th"))
  suffix <- switch(as.character(n %% 10),
                   "1" = "st",
                   "2" = "nd",
                   "3" = "rd",
                   "th")
  return(paste0(n, suffix))
}

init_xc_data <- cross_country_data |>
  clean_names() |>
  select(event,
         athlete,
         school,
         class,
         distance,
         place = pl,
         time,
         pts) |>
  mutate(
    time_new = str_pad(time, width = 7, side = 'left', pad = '0')
  ) |>
  separate(time_new, into = c("mins", "secs"), sep = ":", convert = TRUE, remove = FALSE) |>
  mutate(
    time_decimal = mins + (secs / 60),
    time_decimal_seconds = time_decimal * 60
  ) |>
  select(-mins, -secs) |>
  mutate(distance_m = readr::parse_number(distance)) |>
  arrange(athlete, 
          distance_m) |> 
  group_by(event,athlete) |>
  mutate(
    segment_km = (distance_m - lag(distance_m, default = 0)) / 1000,
    segment_time_min = time_decimal - lag(time_decimal, default = 0),
    pace_km_min = segment_time_min / segment_km,
    pace_mi_min = pace_km_min * 1.60934,
    pace_km_formatted = sprintf("%d:%04.1f", floor(pace_km_min), (pace_km_min %% 1) * 60),
    pace_mi_formatted = sprintf("%d:%04.1f", floor(pace_mi_min), (pace_mi_min %% 1) * 60)
  )