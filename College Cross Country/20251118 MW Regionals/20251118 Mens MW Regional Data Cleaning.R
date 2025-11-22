
####  NCAA Midwest Regional Cross Country 2025

# ----  libraries ----
library(ggplot2)
library(tidyverse)
library(janitor)
library(readxl)

# ----  data pull  ----
mens_midwest_regionals_25 <- read_xlsx("C:/Users/alexe/OneDrive/Desktop/xc midwest results 2025.xlsx")

# ----  functions ----
time_to_decimal <- function(time_string) {
  parts <- as.numeric(strsplit(time_string, ":")[[1]])
  minutes <- parts[1]
  seconds <- parts[2]
  return(minutes + seconds/60)
}

decimal_to_time <- function(decimal_mins) {
  # Vectorized version - handle vectors of values
  sign <- ifelse(decimal_mins < 0, "-", "")
  decimal_mins_abs <- abs(decimal_mins)
  minutes <- floor(decimal_mins_abs)
  seconds <- (decimal_mins_abs - minutes) * 60
  return(sprintf("%s%d:%04.1f", sign, minutes, seconds))
}

time_to_seconds <- function(time_str) {
  parts <- str_split(time_str, ":")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}


# ----  data cleaning ----
mens_mw_clean <- mens_midwest_regionals_25 |>
  clean_names() |>
  # Pivot times
  pivot_longer(
    cols = matches("^x\\d+m$"),  # Matches columns like x2000m, x3300m, etc.
    names_to = "split",
    values_to = "time"
  ) |>
  # Pivot places
  pivot_longer(
    cols = starts_with("place_"),
    names_to = "place_split",
    values_to = "place"
  ) |>
  # Extract the distance from both split columns
  mutate(
    split_distance = str_extract(split, "\\d+"),
    place_distance = str_extract(place_split, "\\d+")
  ) |>
  # Keep only matching split-place pairs
  filter(split_distance == place_distance) |>
  # Clean up
  select(-split_distance, -place_distance, -place_split) |>
  # Format times
  mutate(
    time_formatted = format(time, "%M:%OS1"),
    time_formatted = sub("^0", "", time_formatted),
    time_decimal = sapply(time_formatted, time_to_decimal),
    split = as.numeric(gsub('[xm]','',split))
  ) |>
  group_by(school, athlete) |>
  arrange(school, athlete, split) |>
  mutate(
    split_diff_decimal = time_decimal - lag(time_decimal),
    split_diff = ifelse(!is.na(split_diff_decimal), 
                        decimal_to_time(split_diff_decimal),
                        NA_character_),
    place_diff = lag(place)-place
  ) |>
  ungroup() |>
  select(-time)