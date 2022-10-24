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

wins_6_fill_scale <- c('white', '#fee8c8', '#fdbb84', '#e34a33')
wins_out_fill_scale <- c('white', '#ece7f2', '#a6bddb', '#2b8cbe')

# loading the data  ----
max_iter_wk <- max(weekly_forecast$wk)
#max_iter_wk <- 1

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

pivot_games_won <- range_games_won %>%
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
  ungroup() %>%
  select(-wk)
  
# the table for each conference
conf_var <- 'SEC'

filter_conf_table <- latest_forecast %>%
  filter(conf == conf_var) %>%
  select(-conf,
         -change_bowl) %>%
  rename(School = school,
         `Record` = current_record,
         `Predicted Record` = predicted_record,
         `Wins 6` = bowl_times,
         `Finishes Out` = runs_table) %>%
  inner_join(pivot_games_won,
             by = c('School' = 'school'))

max_6_wins <- max(filter_conf_table$`Wins 6`)
max_finishes_out <- max(filter_conf_table$`Finishes Out`)
max_percent_win <- max(c(filter_conf_table$`0`,filter_conf_table$`1`,filter_conf_table$`2`,filter_conf_table$`3`,filter_conf_table$`4`,filter_conf_table$`5`,filter_conf_table$`6`,filter_conf_table$`7`,filter_conf_table$`8`,filter_conf_table$`9`,filter_conf_table$`10`,filter_conf_table$`11`,filter_conf_table$`12`))

overall_table <- filter_conf_table %>%
  gt() %>%
  # 
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
              table.font.size = 12) 

wins_6_table <- overall_table %>%
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
      rows = `Wins 6` == 100, 
      fns = percent_100) %>%
  fmt("Wins 6", 
      rows = `Wins 6` < 99 & `Wins 6` >= 1, 
      fns = nor_percent)  


finishes_out_table <- wins_6_table %>%
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
      rows = `Finishes Out` == 100, 
      fns = percent_100) %>%
  fmt("Finishes Out", 
      rows = `Finishes Out` < 99 & `Finishes Out` >= 1, 
      fns = nor_percent)
  
range_wins_table <- finishes_out_table %>%
  fmt("0", 
      rows = `0` < 1, 
      fns = less_1) %>%
  fmt("0", 
      rows = `0` == 0, 
      fns = no_percent) %>%
  fmt("0", 
      rows = `0` > 99, 
      fns = greater_99) %>%
  fmt("0", 
      rows = `0` == 100, 
      fns = percent_100) %>%
  fmt("0", 
      rows = `0` < 99 & `0` >= 1, 
      fns = nor_percent) %>%
  fmt("1", 
      rows = `1` < 1, 
      fns = less_1) %>%
  fmt("1", 
      rows = `1` == 0, 
      fns = no_percent) %>%
  fmt("1", 
      rows = `1` > 99, 
      fns = greater_99) %>%
  fmt("1", 
      rows = `1` == 100, 
      fns = percent_100) %>%
  fmt("1", 
      rows = `1` < 99 & `1` >= 1, 
      fns = nor_percent) %>%
  fmt("2", 
      rows = `2` < 1, 
      fns = less_1) %>%
  fmt("2", 
      rows = `2` == 0, 
      fns = no_percent) %>%
  fmt("2", 
      rows = `2` > 99, 
      fns = greater_99) %>%
  fmt("2", 
      rows = `2` == 100, 
      fns = percent_100) %>%
  fmt("2", 
      rows = `2` < 99 & `2` >= 1, 
      fns = nor_percent) %>%
  fmt("3", 
      rows = `3` < 1, 
      fns = less_1) %>%
  fmt("3", 
      rows = `3` == 0, 
      fns = no_percent) %>%
  fmt("3", 
      rows = `3` > 99, 
      fns = greater_99) %>%
  fmt("3", 
      rows = `3` == 100, 
      fns = percent_100) %>%
  fmt("3", 
      rows = `3` < 99 & `3` >= 1, 
      fns = nor_percent) %>%
  fmt("4", 
      rows = `4` < 1, 
      fns = less_1) %>%
  fmt("4", 
      rows = `4` == 0, 
      fns = no_percent) %>%
  fmt("4", 
      rows = `4` > 99, 
      fns = greater_99) %>%
  fmt("4", 
      rows = `4` == 100, 
      fns = percent_100) %>%
  fmt("4", 
      rows = `4` < 99 & `4` >= 1, 
      fns = nor_percent) %>%
  fmt("5", 
      rows = `5` < 1, 
      fns = less_1) %>%
  fmt("5", 
      rows = `5` == 0, 
      fns = no_percent) %>%
  fmt("5", 
      rows = `5` > 99, 
      fns = greater_99) %>%
  fmt("5", 
      rows = `5` == 100, 
      fns = percent_100) %>%
  fmt("5", 
      rows = `5` < 99 & `5` >= 1, 
      fns = nor_percent) %>%
  fmt("6", 
      rows = `6` < 1, 
      fns = less_1) %>%
  fmt("6", 
      rows = `6` == 0, 
      fns = no_percent) %>%
  fmt("6", 
      rows = `6` > 99, 
      fns = greater_99) %>%
  fmt("6", 
      rows = `6` == 100, 
      fns = percent_100) %>%
  fmt("6", 
      rows = `6` < 99 & `6` >= 1, 
      fns = nor_percent) %>%
  fmt("7", 
      rows = `7` < 1, 
      fns = less_1) %>%
  fmt("7", 
      rows = `7` == 0, 
      fns = no_percent) %>%
  fmt("7", 
      rows = `7` > 99, 
      fns = greater_99) %>%
  fmt("7", 
      rows = `7` == 100, 
      fns = percent_100) %>%
  fmt("7", 
      rows = `7` < 99 & `7` >= 1, 
      fns = nor_percent) %>%
  fmt("8", 
      rows = `8` < 1, 
      fns = less_1) %>%
  fmt("8", 
      rows = `8` == 0, 
      fns = no_percent) %>%
  fmt("8", 
      rows = `8` > 99, 
      fns = greater_99) %>%
  fmt("8", 
      rows = `8` == 100, 
      fns = percent_100) %>%
  fmt("8", 
      rows = `8` < 99 & `8` >= 1, 
      fns = nor_percent) %>%
  fmt("9", 
      rows = `9` < 1, 
      fns = less_1) %>%
  fmt("9", 
      rows = `9` == 0, 
      fns = no_percent) %>%
  fmt("9", 
      rows = `9` > 99, 
      fns = greater_99) %>%
  fmt("9", 
      rows = `9` == 100, 
      fns = percent_100) %>%
  fmt("9", 
      rows = `9` < 99 & `9` >= 1, 
      fns = nor_percent) %>%
  fmt("10", 
      rows = `10` < 1, 
      fns = less_1) %>%
  fmt("10", 
      rows = `10` == 0, 
      fns = no_percent) %>%
  fmt("10", 
      rows = `10` > 99, 
      fns = greater_99) %>%
  fmt("10", 
      rows = `10` == 100, 
      fns = percent_100) %>%
  fmt("10", 
      rows = `10` < 99 & `10` >= 1, 
      fns = nor_percent) %>%
  fmt("11", 
      rows = `11` < 1, 
      fns = less_1) %>%
  fmt("11", 
      rows = `11` == 0, 
      fns = no_percent) %>%
  fmt("11", 
      rows = `11` > 99, 
      fns = greater_99) %>%
  fmt("11", 
      rows = `11` == 100, 
      fns = percent_100) %>%
  fmt("11", 
      rows = `11` < 99 & `11` >= 1, 
      fns = nor_percent) %>%
  fmt("12", 
      rows = `12` < 1, 
      fns = less_1) %>%
  fmt("12", 
      rows = `12` == 0, 
      fns = no_percent) %>%
  fmt("12", 
      rows = `12` > 99, 
      fns = greater_99) %>%
  fmt("12", 
      rows = `12` == 100, 
      fns = percent_100) %>%
  fmt("12", 
      rows = `12` < 99 & `12` >= 1, 
      fns = nor_percent)

add_fill_table <- range_wins_table %>%
  data_color(
    columns = `Wins 6`,
    colors = scales::col_numeric(
      palette = wins_6_fill_scale,
      domain = c(0, max_6_wins)
    )
  ) %>%
  data_color(
    columns = `Finishes Out`,
    colors = scales::col_numeric(
      palette = wins_out_fill_scale,
      domain = c(0, max_finishes_out)
    )
  )  %>%
  data_color(
    columns = c(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`),
    colors = scales::col_numeric(
      palette = wins_6_fill_scale,
      domain = c(0, max_percent_win)
    )
  )  

add_gray_table <- add_fill_table %>%
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
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `0`,
      rows = `0` < 1
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `1`,
      rows = `1` < 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `2`,
      rows = `2` < 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `3`,
      rows = `3` < 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `4`,
      rows = `4` < 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `5`,
      rows = `5` < 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `6`,
      rows = `6` < 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `7`,
      rows = `7` < 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `8`,
      rows = `8` < 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `9`,
      rows = `9` < 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `10`,
      rows = `10` < 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `11`,
      rows = `11` < 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `12`,
      rows = `12` < 1
    )
  )

add_black_table <- add_gray_table %>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `Wins 6`,
      rows = `Wins 6` >= 1
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `Finishes Out`,
      rows = `Finishes Out` >= 1
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `0`,
      rows = `0` >= 1
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `1`,
      rows = `1` >= 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `2`,
      rows = `2` >= 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `3`,
      rows = `3` >= 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `4`,
      rows = `4` >= 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `5`,
      rows = `5` >= 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `6`,
      rows = `6` >= 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `7`,
      rows = `7` >= 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `8`,
      rows = `8` >= 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `9`,
      rows = `9` >= 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `10`,
      rows = `10` >= 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `11`,
      rows = `11` >= 1
    )
  )%>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `12`,
      rows = `12` >= 1
    )
  )