library(data.table)
library(tidyverse)
library(ggplot2)
library(glue)

db1b <- list.files('~/GitHub/Sports/College Football ELO Model/Test Data', pattern = "*.csv", full.names = TRUE)
db1b_DF <- as.data.frame(rbindlist(lapply(db1b, fread)))

db1b_DF %>%
  separate(group, into = c('lower', 'upper'), sep = ',') %>%
  mutate(lower = as.numeric(gsub('\\(', '', lower)),
         upper = as.numeric(gsub('\\]', '', upper)),
         median = (upper+lower)/2, 
         distance = abs(pct-median)) %>%
  group_by(test_no,
           k_val,
           home_adv,
           regress_val) %>%
  summarise(#median_dist = round(median(distance),4),
            #mean_dist = round(mean(distance),4),
            median_brier = round(median(median_brier),4),
            mean_brier = round(mean(mean_brier),4)) %>%
  ungroup() %>%
  arrange(median_brier) %>%
  as.data.frame()