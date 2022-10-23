# prediction table outputs for college football model

library(gt)

options(scipen = 999)

# percent symbols to use for the prediction tables  ----
less_1 <- function(.x) {
  "<1%"
}

greater_99 <- function(.x) {
  ">99%"
}

no_percent <- function(.x) {
  "-"
}

percent_100 <- function(.x) {
  "\u2713"
}

# loading the data  ----
max_iter_wk <- max(weekly_forecast$wk)

# preparing the data for the table  ----

latest_forecast <- weekly_forecast %>%
  mutate(bowl_times = round((bowl_times/n_times)*100,3),
         win_out_times = round((win_out_times/n_times)*100,3),
         runs_table = round((runs_table/n_times)*100,3)) %>%
  group_by(school) %>%
  arrange(wk) %>%
  mutate(change_bowl = bowl_times-lag(bowl_times)) %>%
  filter(wk == max_iter_wk) %>%
  arrange(desc(bowl_times),
          desc(runs_table)) %>%
  unite(current_record, c('c_wins', 'c_lose'), sep = '-') %>%
  unite(predicted_record, c('avg_wins', 'avg_loses'), sep = '-')

