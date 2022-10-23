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

nor_percent <- function(.x) {
  glue::glue("{.x}%")
}

fill_scale <- c('#fee8c8', '#fdbb84', '#e34a33')

# loading the data  ----
#max_iter_wk <- max(weekly_forecast$wk)
max_iter_wk <- 1

# preparing the data for the table  ----

latest_forecast <- weekly_forecast %>%
  mutate(bowl_times = round((bowl_times/n_times)*100,3),
         win_out_times = round((win_out_times/n_times)*100,3),
         runs_table = round((runs_table/n_times)*100,3)) %>%
  group_by(school) %>%
  arrange(wk) %>%
  mutate(change_bowl = bowl_times-lag(bowl_times)) %>%
  ungroup() %>%
  filter(wk == max_iter_wk) %>%
  arrange(desc(bowl_times),
          desc(runs_table)) %>%
  unite(current_record, c('c_wins', 'c_lose'), sep = '-') %>%
  unite(predicted_record, c('avg_wins', 'avg_loses'), sep = '-') %>%
  select(conf,
         school,
         current_record,
         predicted_record,
         bowl_times,
         change_bowl,
         runs_table)

range_games_won %>%
  select(-conf,
         -div,
         -freq) %>%
  filter(wk == max_iter_wk) %>%
  mutate(freq_p = round((freq_p)*100,3)) %>%
  group_by(school,
           wk) %>%
  complete(total_wins = seq(0, 12, 1)) %>%
  replace(is.na(.), 0) %>%
  pivot_wider(names_from = total_wins, values_from = freq_p) %>%
  ungroup() 
  
# the table for each conference
conf_var <- 'MAC'

filter_conf_table <- latest_forecast %>%
  filter(conf == conf_var) %>%
  select(-conf,
         -change_bowl) %>%
  rename(School = school,
         `Record` = current_record,
         `Predicted Record` = predicted_record,
         `Wins 6` = bowl_times,
         `Finishes Out` = runs_table)

max_6_wins <- max(filter_conf_table$`Wins 6`)

filter_conf_table %>%
  gt() %>%
  tab_options(
    column_labels.border.top.style = "none",
    table.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    column_labels.border.bottom.width = 1,
    column_labels.border.bottom.color = "#334422",
    table_body.border.top.style = "none",
    table_body.border.bottom.color = "#0000001A",
    data_row.padding = px(7)
  ) %>%
  tab_style(
    style = cell_text(
      size = px(12),
      weight = "bold",
      transform = "uppercase"
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(table.font.names = "IBM Plex Sans",
              table.font.size = 12) %>%
  fmt("Wins 6", 
      rows = `Wins 6` < 1, 
      fns = less_1) %>%
  fmt("Wins 6", 
      rows = `Wins 6` == 0, 
      fns = no_percent) %>%
  fmt("Wins 6", 
      rows = `Wins 6` > 99, 
      fns = greater_99) %>%
  fmt("Wins 6", 
      rows = `Wins 6` < 99 & `Wins 6` >= 1, 
      fns = nor_percent)  %>%
  fmt("Finishes Out", 
      rows = `Finishes Out` < 1, 
      fns = less_1) %>%
  fmt("Finishes Out", 
      rows = `Finishes Out` == 0, 
      fns = no_percent) %>%
  fmt("Finishes Out", 
      rows = `Finishes Out` > 99, 
      fns = greater_99) %>%
  fmt("Finishes Out", 
      rows = `Finishes Out` < 99 & `Finishes Out` >= 1, 
      fns = nor_percent) %>%
  data_color(
    columns = `Wins 6`,
    colors = scales::col_numeric(
      palette = fill_scale,
      domain = c(0, max_6_wins)
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `Wins 6`,
      rows = `Wins 6` < 1
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `Finishes Out`,
      rows = `Finishes Out` < 1
    )
  )