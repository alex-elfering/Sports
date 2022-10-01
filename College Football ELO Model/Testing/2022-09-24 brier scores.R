# summarizes the average brier score for every variable test done for the College Football ELO Model

library(data.table)
library(tidyverse)
library(ggplot2)
library(glue)
library(ggbeeswarm)

options(scipen = 999)

test_scores <- list.files('~/GitHub/Sports/College Football ELO Model/Testing/Test Data', pattern = "*.csv", full.names = TRUE)
test_scores_df <- as.data.frame(rbindlist(lapply(test_scores, fread)))

brier_scores <- test_scores_df %>%
  filter(wins == 1) %>%
  mutate(brier = ifelse(wins == 1, brier, NA)) %>%
  separate(group, into = c('lower', 'upper'), sep = ',') %>%
  mutate(lower = as.numeric(gsub('\\(', '', lower)),
         upper = as.numeric(gsub('\\]', '', upper))) %>%
  group_by(test_no,
           k_val,
           home_adv,
           regress_val) %>%
  summarise(brier_mean_overall = mean(brier)) %>%
  ungroup() %>%
  arrange(brier_mean_overall)

brier_k <- brier_scores %>%
  group_by(k_val) %>%
  summarise(min_b = min(brier_mean_overall),
            median_b = median(brier_mean_overall),
            mean_b = mean(brier_mean_overall),
            max_b = max(brier_mean_overall)) %>%
  ungroup()

brier_adv <- brier_scores %>%
  group_by(home_adv) %>%
  summarise(min_b = min(brier_mean_overall),
            median_b = median(brier_mean_overall),
            mean_b = mean(brier_mean_overall),
            max_b = max(brier_mean_overall)) %>%
  ungroup()

brier_regress <- brier_scores %>%
  group_by(regress_val) %>%
  summarise(min_b = min(brier_mean_overall),
            median_b = median(brier_mean_overall),
            mean_b = mean(brier_mean_overall),
            max_b = max(brier_mean_overall)) %>%
  ungroup()

brier_k
brier_adv
brier_regress

brier_scores_chart <- ggplot(brier_scores, 
                             aes(x = NA, 
                                 y = round(brier_mean_overall, 5))) +
  geom_quasirandom(size = 8,
                   cex = 4,
                   color = 'steelblue',
                   #alpha = 0.7,
                   priority = "density") +
  geom_quasirandom(size = 6,
                   cex = 4,
                   color = 'white',
                   #alpha = 0.7,
                   priority = "density") +
  #coord_flip() +
  labs(x = '',
       y = '',
       #title = 'Average Brier Score of Variable Testing in CFB ELO Forecast',
       subtitle = '') +
  theme(plot.title = element_text(face = 'bold', size = 18),
        plot.subtitle = element_text(size = 16),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y.left = element_text(size = 12, color = 'black'),
        axis.text.x.bottom = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 

ggsave(brier_scores_chart, file = '~/GitHub/Sports/College Football ELO Model/brier_scores_chart.png', width = 17, height = 7, units = c('in'))
