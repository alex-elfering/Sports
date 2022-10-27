# charts and tables to breakdown bowl predictions in college football forecast

library(ggplot2)
library(gt)
library(tidyverse)

weekly_forecast <- read.csv('~/GitHub/Sports/College Football Bowl Predictions/Data/CFB Game Predictions.csv')

# how many teams will make a bowl?

bowl_eligible_teams <- weekly_forecast %>%
  mutate(bowl_team = ifelse(avg_wins/(avg_wins + avg_loses) >= 0.5, 1, 0)) %>%
  group_by(wk) %>%
  summarise(bowl_teams = sum(bowl_team)) %>%
  ungroup()

bowl_eligible_teams %>%
  ggplot() + 
  geom_bar(mapping = aes(x = wk,
                         y = bowl_teams),
           stat = 'identity',
           position = 'identity',
           width = 0.5)



schools <- unique(weekly_forecast$school)

for(g in schools){
  
print(g)
  
  filter_school <- weekly_forecast %>%
    filter(school == g) %>%
    mutate(bowl_times = bowl_times/n_times,
           runs_table = runs_table/n_times)
  
  school_bowls <- filter_school %>%
    ggplot() + 
    geom_step(mapping = aes(x = wk,
                            y = bowl_times),
              color = 'steelblue',
              alpha = 0.6,
              size = 4) +
    geom_point(mapping = aes(x = wk,
                             y = bowl_times),
               color = 'steelblue',
               size = 6) +
    geom_point(mapping = aes(x = wk,
                             y = bowl_times),
               color = 'white',
               shape = 1,
               size = 6) +
    labs(title = paste('The likelihood that ', g, ' wins at least 6 games', sep = ''),
         caption = 'Code and visualization by Alex Elfering | Source: College Football Reference',
         x = '',
         y = '') +
    scale_y_continuous(limits = c(0,1),
                       labels = scales::percent) +
    scale_x_continuous(#limits = c(0,9),
                       labels = seq(1,9, 2)) +
    theme(plot.title = element_text(face = 'bold', size = 14),
          plot.subtitle = element_text(face = 'bold', size = 12),
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
  
}