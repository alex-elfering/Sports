# visualization to compare selected athlete vs. teammates or top 'n'
# using a distribution and a table for summarizing

source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251118 Mens MW Regional Data Cleaning.R')
source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251129 school colors.R')

library(ggplot2)
library(ggrepel)
library(ggbeeswarm)
library(glue)
library(gt)
library(colorspace)
library(ggpattern)

# variable testing

athlete_var <- mens_mw_clean |>
  select(athlete) |>
  distinct() |>
  sample_n(1) |>
  pull(athlete)

school_var <- mens_mw_clean |>
  filter(athlete == athlete_var) |>
  distinct(school) |>
  pull(school)

top_n <- 25

# data frames needed(?)

teammates_10k_var <- mens_mw_clean |>
  filter(school == school_var,
         athlete != athlete_var) |>
  filter(split == 10000) |>
  pull(athlete)

top_n_10k_var <- mens_mw_clean |>
  filter(split == 10000,
         #  just in case they are in the top 'n'
         athlete != athlete_var) |>
  filter(dense_rank((time_decimal)) <= top_n) |>
  pull(athlete)

school_color_var <- school_colors |>
  filter(school == school_var) |>
  pull(primary_color)

school_color_var_light <- lighten(school_color_var, amount = 0.5)

mens_10k_times <- mens_mw_clean |>
  filter(split == 10000) |>
  select(school,
         athlete,
         time_decimal,
         place) |>
  mutate(color_spotlight = case_when(athlete == athlete_var ~ 'spotlight',
                                     athlete %in% teammates_10k_var ~ 'teammates',
                                     athlete %in% top_n_10k_var ~ 'top_n',
                                     TRUE ~ 'other'),
         color_spotlight = factor(color_spotlight, levels = c('spotlight','teammates','top_n','other')))

# visualization ideas 

#mens_10k_times |>
#  ggplot() + 
#  geom_histogram(aes(time_decimal,
#                     group = athlete,
#                     fill = color_spotlight,
#                     color = color_spotlight == "spotlight"),
#                 #color = 'white',
#                 binwidth = 0.25) +
#  scale_x_reverse() +
#  scale_color_manual(
#    values = c(
#      'TRUE' = 'black',
#      'FALSE' = 'white'
#    )
#  ) +
#  scale_fill_manual(
#    values = c(
#      "spotlight" = school_color_var,
#      "teammates" = school_color_var_light,
#      "top_n" = "gray10",  # Gold for top finishers
#      "other" = "gray90"
#    ),
#    labels = c(
#      "spotlight" = athlete_var,
#      "teammates" = glue("{school_var} teammates"),
#      "top_n" = glue("Top {top_n} finishers"),
#      "other" = "Other runners"
#    )
#  ) 

mens_10k_times |>
  ggplot(aes(x = '', y = time_decimal)) + 
  geom_beeswarm(
    aes(fill = color_spotlight,
        color = color_spotlight == "spotlight",
        size = color_spotlight == "spotlight"),
    shape = 21,
    stroke = 1,
    cex = 2.3,
    method = 'center'
  ) +
  coord_flip() +
  scale_y_reverse() +
  scale_color_manual(
    values = c(
      'TRUE' = 'black',
      'FALSE' = 'white'
    ),
    guide = 'none'
  ) +
  scale_size_manual(
    values = c(
      'TRUE' = 4,      # Larger for spotlight
      'FALSE' = 3
    ),
    guide = 'none'
  ) +
  scale_fill_manual(
    values = c(
      "spotlight" = school_color_var,
      "teammates" = school_color_var_light,
      "top_n" = "gray10",
      "other" = "gray90"
    ),
    labels = c(
      "spotlight" = athlete_var,
      "teammates" = glue("{school_var} teammates"),
      "top_n" = glue("Top {top_n} finishers"),
      "other" = "Other runners"
    )
  )
