library(tidyverse)
library(janitor)
library(readxl)
library(data.table)

decimal_to_time <- function(x) {
  mins <- floor(x)
  secs <- round((x - mins) * 60, 1)
  sprintf("%d:%05.2f", mins, secs)
}

format_time_diff <- function(diff_seconds) {
  abs_diff <- abs(diff_seconds)
  if (abs_diff >= 60) {
    mins <- abs_diff %/% 60  # Use integer division instead of floor
    secs <- round(abs_diff %% 60)
    sprintf("%d:%02d", mins, secs)
  } else {
    paste0(round(abs_diff), " seconds")
  }
}

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
