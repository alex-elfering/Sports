# prediction table outputs for college football model

library(gt)

options(scipen = 999)

setwd("~/GitHub/Sports/College Football Bowl Predictions/Conference Tables")

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
max_iter_wk <- 9

# preparing the data for the table  ----

latest_forecast <- weekly_forecast %>%
  mutate(bowl_times = round((bowl_times/n_times)*100,3),
         win_out_times = round((win_out_times/n_times)*100,3),
         runs_table = round((runs_table/n_times)*100,3),
         games_13 = ifelse(avg_wins + avg_loses == 13, 1, 0)) %>%
  group_by(school) %>%
  arrange(wk) %>%
  mutate(change_bowl = bowl_times-lag(bowl_times)) %>%
  ungroup() %>%
  filter(wk == max_iter_wk) %>%
  arrange(desc(bowl_times),
          desc(avg_wins),
          desc(runs_table)) %>%
  unite(current_record, c('c_wins', 'c_lose'), sep = '-') %>%
  unite(predicted_record, c('avg_wins', 'avg_loses'), sep = '-') %>%
  select(conf,
         school,
         games_13,
         current_record,
         predicted_record,
         bowl_times,
         change_bowl,
         runs_table)
  
# the table for each conference----

for(i in unique(latest_forecast$conf)){
  
  print(i)
  
filter_conf_table <- latest_forecast %>%
  filter(conf == i) %>%
  select(-conf,
         -change_bowl) %>%
  #mutate(school = ifelse(games_13 == 1, paste0(school, '*'), school)) %>%
  #select(-games_13) %>%
  rename(School = school,
         `Record` = current_record,
         `Predicted Record` = predicted_record,
         `Wins 6` = bowl_times,
         `Finishes Out` = runs_table) 

final_game <- latest_forecast %>%
  filter(conf == i) %>%
  summarise(max_games = max(games_13))

set_max_games <- as.numeric(ifelse(final_game == 1, 13, 12))

statement_13_games <- ifelse(set_max_games == 13, ' | *Team plays an extra game', '')

filter_games_won <- range_games_won %>%
  filter(wk == max_iter_wk,
         conf == i) 

pivot_games_won <- filter_games_won %>%
  select(-conf,
         -div,
         -freq) %>%
  mutate(freq_p = round((freq_p)*100,3)) %>%
  group_by(school,
           wk) %>%
  complete(total_wins = seq(0, set_max_games, 1)) %>%
  replace(is.na(.), 0) %>%
  pivot_wider(names_from = total_wins, values_from = freq_p) %>%
  ungroup() %>%
  select(-wk)

final_conf_table <- filter_conf_table %>%
  inner_join(pivot_games_won, by = c('School' = 'school')) %>%
  mutate(School = ifelse(games_13 == 1, paste0(School, '*'), School)) %>%
  select(-games_13)

max_6_wins <- max(final_conf_table$`Wins 6`)
max_finishes_out <- max(final_conf_table$`Finishes Out`)
max_percent_win <- max(filter_games_won$freq_p)

overall_table <- final_conf_table %>%
  gt() %>%
  # 
  tab_options(
    column_labels.border.top.style = "none",
    table.border.top.style = "none",
    #column_labels.border.bottom.style = "none",
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
              table.font.size = 12)  %>%
  tab_spanner(
    label = "Likelihood Team...",
    columns = c(
      'Wins 6', 'Finishes Out'
    )
  ) %>%
  tab_spanner(
    label = "Likelihood Team Wins...",
    columns = -c('Wins 6', 'Finishes Out', 'School', 'Record', 'Predicted Record')
    ) %>%
  cols_width(
    School ~ px(800)
  ) %>%
  tab_header(
    title = md(glue("**{i} Game Predictions**")),
    subtitle = glue("{season_vari} Season as of Week #{max_iter_wk}")
  ) %>%
  tab_source_note(
    source_note = glue('Predicted records are calculated using ELO ratings that score each team based on factors such as home-field advantage, margin of victory, and quality of opponent. At the end of each season, school ratings regress partially to the value of their respective conference. Teams new to FBS begin with an ELO rating of 1500. Games are simulated {n_times} and averaged to simulate potential wins and losses.')
  ) %>%
  tab_source_note(
    source_note = glue("Code by Alex Elfering | Source: College Football Reference | Model Inspired by FiveThirtyEight{statement_13_games}")
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = 'black',
      weight = px(1)),
    locations = cells_body(
      columns = c(`Finishes Out`)
    )
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = 'black',
      style = 'dashed',
      #linetype = 'dashed',
      weight = px(1)),
    locations = cells_body(
      columns = c(`5`)
    )
  )

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
  
if(set_max_games == 13){
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
      fns = nor_percent) %>%
  fmt("13", 
      rows = `13` < 1, 
      fns = less_1) %>%
  fmt("13", 
      rows = `13` == 0, 
      fns = no_percent) %>%
  fmt("13", 
      rows = `13` > 99, 
      fns = greater_99) %>%
  fmt("13", 
      rows = `13` == 100, 
      fns = percent_100) %>%
  fmt("13", 
      rows = `13` < 99 & `13` >= 1, 
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
    columns = c(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`, `13`),
    colors = scales::col_numeric(
      palette = wins_6_fill_scale,
      domain = c(0, round((max_percent_win)*100,3))
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
  )%>%
  tab_style(
    style = list(
      cell_text(color = "gray75")
    ),
    locations = cells_body(
      columns = `13`,
      rows = `13` < 1
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
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "black")
    ),
    locations = cells_body(
      columns = `13`,
      rows = `13` >= 1
    )
  ) %>%
  gtsave(glue("{i} Week#{max_iter_wk} game predictions.png"), vwidth = 1100)
}else{
  
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
    columns = c(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`),
    colors = scales::col_numeric(
      palette = wins_6_fill_scale,
      domain = c(0, round((max_percent_win)*100,3))
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
  ) %>%
  gtsave(glue("{i} Week#{max_iter_wk} game predictions.png"), vwidth = 1100)}
}