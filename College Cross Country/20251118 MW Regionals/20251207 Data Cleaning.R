# combining multiple cross country event datasets
# let's start with midwest regionals...for now

library(tidyverse)
library(janitor)
library(readxl)
library(data.table)

setwd("C:/Users/alexe/OneDrive/Desktop/cross country raw data")
xc_races_file_list <- list.files("C:/Users/alexe/OneDrive/Desktop/cross country raw data", pattern = "*.csv")
xc_data_list <- (lapply(xc_races_file_list, fread))

test_file <- read_xlsx('2025 mens midwest regionals xc.xlsx')

# data cleaning functions ----

source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251207 XC Data Cleaning Functions.R')

# data cleaning steps: output is one single file with every event and respective splits all in the same unit of measurement ----

clean_cross_country_data <- function(df){
  pivot_xc <- df |>
    clean_names() |>
    # Pivot times
    pivot_longer(
      cols = matches("^x\\d+M$"),  
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
      time_formatted = as.character(time),  # Just convert to character (it already is one)
      time_formatted = sub("^0", "", time_formatted),  # Remove leading zero
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
    ungroup()
  
  # assign sequence to splits
  
  split_sequence <- pivot_xc |>
    distinct(split) |>
    mutate(split_number = row_number())
  
  final_xc_data_frame <- pivot_xc |>
    inner_join(split_sequence)
  
  return(final_xc_data_frame)
  
}

full_xc_df <- rbindlist(lapply(xc_data_list,clean_cross_country_data))