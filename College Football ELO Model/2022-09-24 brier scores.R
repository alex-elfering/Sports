library(data.table)
library(tidyverse)
library(ggplot2)
library(glue)

db1b <- list.files('~/GitHub/Sports/College Football ELO Model/Test Data', pattern = "*.csv", full.names = TRUE)
db1b_DF <- as.data.frame(rbindlist(lapply(db1b, fread)))

bracket_brier <- db1b_DF %>%
  mutate(brier = ifelse(wins == 1, brier, NA)) %>%
  separate(group, into = c('lower', 'upper'), sep = ',') %>%
  mutate(lower = as.numeric(gsub('\\(', '', lower)),
         upper = as.numeric(gsub('\\]', '', upper))) %>%
  group_by(test_no,
           season,
           lower,
           upper,
           k_val,
           home_adv,
           regress_val) %>%
  summarise(wins = sum(wins),
            games = sum(game),
            median_brier = median(brier, na.rm = TRUE),
            mean_brier = mean(brier, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct = wins/games,
         median = (upper+lower)/2, 
         distance = abs(pct-median))

bracket_brier %>%
  group_by(test_no,
           k_val,
           home_adv,
           regress_val) %>%
  summarise(median_brier = median(median_brier, na.rm = TRUE),
            mean_brier = mean(mean_brier, na.rm = TRUE),
            median_dist = median(distance, na.rm = TRUE),
            mean_dist = mean(distance, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(median_brier) %>%
  filter(row_number() <= 10) %>%
  as.data.frame()