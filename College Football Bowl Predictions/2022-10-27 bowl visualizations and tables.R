# charts and tables to breakdown bowl predictions in college football forecast

library(ggplot2)
library(gt)
library(tidyverse)
library(glue)

weekly_forecast <- read.csv('~/GitHub/Sports/College Football Bowl Predictions/Data/CFB Game Predictions.csv')

max_week <- max(weekly_forecast$wk)

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

weekly_forecast %>%
  select(-X) %>%
  group_by(school) %>%
  mutate(
         prior_bowl = lag(bowl_times)/10000,
         bowl_change = (bowl_times-lag(bowl_times))/10000,
         bowl_times = bowl_times/10000) %>%
  filter(wk == 9) %>%
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
  tab_options(table.font.names = "Fira Code Light",
              table.font.size = 12)

