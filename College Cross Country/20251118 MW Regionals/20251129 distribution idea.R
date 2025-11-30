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
library(showtext)

set.seed(42)

showtext_auto()
showtext_opts(dpi = 300)

# fonts fonts fonts

font_add_google("IBM Plex Sans", "ibm")
font_add_google("Noto Sans","noto")


# variable testing

athlete_var <- 'Denny Chapman'

school_var <- mens_mw_clean |>
  filter(athlete == athlete_var) |>
  distinct(school) |>
  pull(school)

top_n <- 10

# data frames needed(?)

teammates_10k_var <- mens_mw_clean |>
  filter(school == school_var) |>
  filter(split == 10000) |>
  pull(athlete)

top_n_10k_var <- mens_mw_clean |>
  filter(split == 10000) |>
  filter(dense_rank((time_decimal)) <= top_n) |>
  pull(athlete)

school_color_var <- school_colors |>
  filter(school == school_var) |>
  pull(primary_color)

school_color_var_light <- lighten(school_color_var, amount = 0)

mens_10k_times <- mens_mw_clean |>
  filter(split == 10000) |>
  select(school,
         athlete,
         time_decimal,
         place) |>
  mutate(color_spotlight = case_when(athlete == athlete_var ~ 'spotlight',
                                     school == school_var ~ 'teammates',
                                     athlete %in% top_n_10k_var ~ 'top_n',
                                     TRUE ~ 'other'),
         color_spotlight = factor(color_spotlight, levels = c('spotlight','teammates','top_n','other')))

# visualization idea

min_time <- floor(min(mens_10k_times$time_decimal,na.rm = T))
max_time <- round(max(mens_10k_times$time_decimal,na.rm = T))+1

# Calculate ranges for rectangles
school_range <- mens_10k_times |>
  filter(color_spotlight %in% c("teammates",'spotlight')) |>
  summarise(
    ymin = min(time_decimal, na.rm = TRUE),
    ymax = max(time_decimal, na.rm = TRUE),
    median = median(time_decimal,na.rm = T)
  )

top_n_range <- mens_10k_times |>
  filter(color_spotlight == "top_n") |>
  summarise(
    ymin = min(time_decimal, na.rm = TRUE),
    ymax = max(time_decimal, na.rm = TRUE),
    median = median(time_decimal,na.rm = T)
  )

athlete_times <- mens_10k_times |>
  filter(athlete == athlete_var)|>
  summarise(
    #ymin = min(time_decimal, na.rm = TRUE),
    #ymax = max(time_decimal, na.rm = TRUE),
    time_stat = median(time_decimal,na.rm = T)
  )

quasi_chart <- mens_10k_times |>
  ggplot(aes(x = 0, y = time_decimal)) + 
  geom_quasirandom(
    aes(fill = color_spotlight,
        color = color_spotlight == "spotlight",
        alpha = color_spotlight,
        size = color_spotlight),
    shape = 21,
    stroke = 1,
    width = 0.4,  # Add width to control spread
    method = 'smiley'
  ) +
  geom_text(
    aes(label = ifelse(athlete == athlete_var, glue('{athlete}:\n{decimal_to_time(athlete_times$time_stat)}'), NA)),
    position = position_quasirandom(width = 0.4, method = 'smiley'),  # Match the geom
    size = 2,
    fontface = "bold",
    #seed = 42,
    family = 'noto',
    hjust = 1,
    vjust = 0
  ) +
  coord_flip() +
  scale_y_reverse(
    limits = rev(c(min_time, max_time)),
    breaks = (seq(min_time, max_time, by = 3)),
    labels = c('29', '32', '35', '38', '41 minutes')  # Fixed order
  ) +
  scale_color_manual(
    values = c('TRUE' = 'black', 'FALSE' = 'white'),
    guide = 'none'
  ) +
  scale_alpha_manual(
    values = c(
      "spotlight" = 1,
      "teammates" = 1,
      "top_n" = 0.8,
      "other" = 0.6
    )
  ) +
  scale_size_manual(
    values = c(
      "spotlight" = 4,
      "teammates" = 4,
      "top_n" = 2.8,
      "other" = 2.5
    )
  ) +
  scale_fill_manual(
    values = c(
      "spotlight" = school_color_var,
      "teammates" = school_color_var_light,
      "top_n" = "gray30",
      "other" = "gray90"
    ),
    labels = c(
      "spotlight" = athlete_var,
      "teammates" = glue("{school_var}"),
      "top_n" = glue("Top {top_n} Finishers"),
      "other" = "Other runners"
    )
  ) +
  labs(
    x = '',
    y = '10K Race Time\n▲ Slower                          ▼ Faster',
    caption = '\nVisualization by Alex Elfering\nSource: Data manually pulled from NCAA and PrimeTime Timing'
  ) +
  theme(
    legend.position = "top",
    plot.caption = element_text(size = 8, hjust = 0, color = 'gray80'),
    axis.text.x = element_text(size = 10, color = 'gray50'),
    axis.title.y = element_text(angle = 90,, hjust = 1, vjust = 1),
    #axis.text.x = element_blank(),
    legend.title = element_blank(),
    plot.title.position = "plot", 
    plot.caption.position = 'plot',
    axis.line.y.left = element_line(color = 'gray50'),
    axis.line.x.bottom = element_blank(),
    axis.ticks.y = element_blank(), 
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(color = 'gray90', linetype = 'dashed')
  )

# density plot

denstiy_chart <- mens_10k_times |>
  ggplot( aes(x=time_decimal, color=color_spotlight, fill=color_spotlight)) +
  geom_density(alpha=0.6) +
  scale_x_reverse(limits = rev(c(min_time, max_time))) +
  scale_fill_manual(
    values = c(
      "spotlight" = school_color_var,
      "teammates" = school_color_var_light,
      "top_n" = "gray30",
      "other" = "gray90"
    ),
    labels = c(
      "spotlight" = athlete_var,
      "teammates" = glue("{school_var}"),
      "top_n" = glue("Top {top_n} Finishers"),
      "other" = "Other runners"
    )
  ) +
  scale_color_manual(
    values = c(
      "spotlight" = school_color_var,
      "teammates" = school_color_var_light,
      "top_n" = "gray30",
      "other" = "gray90"
    ),
    labels = c(
      "spotlight" = athlete_var,
      "teammates" = glue("{school_var}"),
      "top_n" = glue("Top {top_n} Finishers"),
      "other" = "Other runners"
    )
  ) +
  theme(
    legend.position = "top",
    plot.caption = element_text(size = 8, hjust = 0, color = 'gray80'),
    axis.text.x = element_blank(),
    axis.title.y = element_text(angle = 90,, hjust = 1, vjust = 1),
    #axis.text.x = element_blank(),
    legend.title = element_blank(),
    plot.title.position = "plot", 
    plot.caption.position = 'plot',
    axis.line.y.left = element_line(color = 'gray50'),
    axis.line.x.bottom = element_blank(),
    axis.ticks.y = element_blank(), 
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(color = 'gray90', linetype = 'dashed')
  )