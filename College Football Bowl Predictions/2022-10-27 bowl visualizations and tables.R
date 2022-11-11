# charts and tables to breakdown bowl predictions in college football forecast

library(ggplot2)
library(gt)
library(tidyverse)
library(glue)

setwd("~/GitHub/Sports/College Football Bowl Predictions/Summary Charts")

weekly_forecast <- read.csv('~/GitHub/Sports/College Football Bowl Predictions/Data/CFB Game Predictions.csv')

max_week <- max(weekly_forecast$wk)
l_week <- max_week-1

latest_week_label <- paste0('Week ', max_week)
last_week_label <- paste0('Week ', l_week)

filter_weekly_forecast <- dplyr::filter(weekly_forecast, wk == max_week)

# bowl odds by team
schools <- unique(weekly_forecast$school)

for(g in schools){
  
  print(g)
  
  filter_school <- weekly_forecast %>%
    filter(school == g) %>%
    mutate(bowl_times = bowl_times/10000,
           runs_table = runs_table/10000)
  
  school_bowls <- filter_school %>%
    ggplot() + 
    geom_step(mapping = aes(x = wk,
                            y = bowl_times),
              color = 'steelblue',
              alpha = 0.6,
              size = 2) +
    geom_point(mapping = aes(x = wk,
                             y = bowl_times),
               color = 'steelblue',
               size = 4) +
    geom_point(mapping = aes(x = wk,
                             y = bowl_times),
               color = 'white',
               shape = 1,
               size = 4) +
    labs(title = paste(g, ' Regular Season Outlook', sep = ''),
         subtitle = 'The likelihood of winning at least six games in the regular season',
         caption = 'Code and visualization by Alex Elfering | Source: College Football Reference',
         x = '',
         y = '') +
    scale_y_continuous(limits = c(0,1),
                       labels = scales::percent) +
    scale_x_continuous(limits = c(1,max_week),
                       breaks = seq(1, max_week, 2),
                       labels = seq(1, max_week, 2)) +
    theme(plot.title = element_text(face = 'bold', size = 16),
          plot.subtitle = element_text(size = 12),
          legend.position = 'top',
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.text = element_text(size = 12),
          plot.title.position = "plot",
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12, color = '#969696'),
          axis.text.x.bottom = element_text(size = 12, color = 'black'),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
          strip.background = element_rect(fill = NA),
          panel.background = ggplot2::element_blank(),
          axis.line = element_line(colour = "#222222", linetype = "solid"),
          panel.grid.major.y = element_line(colour = "gray75", linetype = "dashed"),
          panel.grid.major.x = element_blank()) 
  
  print(school_bowls)
  
  ggsave(school_bowls,
         file = glue('~/GitHub/Sports/College Football Bowl Predictions/Team Outlook/{g} regular season outlook.png'), 
         width = 6, 
         height = 4, 
         units = 'in')
  
}

# which teams have seen their bowl odds improve or worsen the most?

nor_percent <- function(.x) {
  glue::glue("{.x}%")
}

plus_percent <- function(.x) {
  glue::glue("+{.x}%")
}

weekly_forecast %>%
  select(-X) %>%
  group_by(school) %>%
  mutate(
    prior_bowl = lag(bowl_times)/10000,
    bowl_change = (bowl_times-lag(bowl_times))/10000,
    bowl_times = bowl_times/10000) %>%
  filter(wk == 11) %>%
  ungroup() %>%
  mutate(desc_rank = dense_rank(desc(bowl_change)),
         asc_rank = dense_rank(bowl_change)) %>%
  filter(desc_rank <= 10 | asc_rank <= 10) %>%
  select(-desc_rank,
         -asc_rank,
         -runs_table,
         -win_out_times,
         -div,
         -wk) %>%
  unite(current_record, c('c_wins', 'c_lose'), sep = '-') %>%
  unite(predicted_record, c('avg_wins', 'avg_loses'), sep = '-') %>%
  arrange(desc(bowl_change)) %>%
  select(school,
         conf,
         current_record,
         predicted_record,
         prior_bowl,
         bowl_times,
         bowl_change) %>%
  mutate(prior_bowl = round(prior_bowl*100, 3),
         bowl_times = round(bowl_times*100, 3),
         bowl_change = round(bowl_change*100, 3)) %>%
rename(`Current Record` = current_record,
       `Predicted Record` = predicted_record,
       !!last_week_label := prior_bowl,
       !!latest_week_label := bowl_times,
       change = bowl_change) %>%
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
              table.font.size = 12) %>%
  fmt(columns = c(5, 6),
      fns = nor_percent)  %>%
  fmt(columns = c(7),
      rows = change < 0,
      fns = nor_percent) %>%
  fmt(columns = c(7),
      rows = change > 0,
      fns = plus_percent) %>%
  tab_style(
    style = list(
      cell_text(color = "darkorange",
                weight = 'bold')
    ),
    locations = cells_body(
      columns = c(7),
      rows = change < 0
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "steelblue",
                weight = 'bold')
    ),
    locations = cells_body(
      columns = c(7),
      rows = change > 0
    )
  ) %>%
  tab_header(
    title = md(glue("**Biggest Movers based on Change in Bowl Odds**"))
  ) %>%
  tab_source_note(
    source_note = glue('Predicted records are calculated using ELO ratings that score each team based on factors such as home-field advantage, margin of victory, and quality of opponent. At the end of each season, school ratings regress partially to the value of their respective conference. Teams new to FBS begin with an ELO rating of 1500. Games are simulated 10,000 times and averaged to simulate potential wins and losses.')
  ) %>%
  tab_source_note(
    source_note = glue("Code by Alex Elfering | Source: College Football Reference | Model Inspired by FiveThirtyEight")
  ) %>%
  gtsave('bowl updates.png')

