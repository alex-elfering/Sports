library(gt)

options(scipen = 999)

max_iter_wk <- max(weekly_forecast$wk)

latest_forecast <- weekly_forecast %>%
  filter(wk == max_iter_wk) %>%
  arrange(desc(bowl_times),
          desc(runs_table)) %>%
  mutate(bowl_times = round((bowl_times/n_times)*100,3),
         win_out_times = round((win_out_times/n_times)*100,3),
         runs_table = round((runs_table/n_times)*100,3)) %>%
  unite(current_record, c('c_wins', 'c_lose'), sep = '-') %>%
  unite(predicted_record, c('avg_wins', 'avg_loses'), sep = '-')

