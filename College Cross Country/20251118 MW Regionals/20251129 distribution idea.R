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
library(shadowtext)

set.seed(42)

showtext_auto()
showtext_opts(dpi = 300)

# fonts fonts fonts

#font_add_google("IBM Plex Sans", "ibm")
#font_add_google("Noto Sans","noto")


# variable testing

athlete_var <- mens_mw_clean |>
  select(athlete) |>
  distinct() |>
  pull(athlete) |>
  sort()

for(i in athlete_var){
  
  school_var <- mens_mw_clean |>
    filter(athlete == i) |>
    distinct(school) |>
    pull(school)
  
  # data frames needed(?)
  
  teammates_10k_var <- mens_mw_clean |>
    filter(school == school_var) |>
    filter(split == 10000) |>
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
    mutate(color_spotlight = case_when(athlete == i ~ 'spotlight',
                                       school == school_var ~ 'teammates',
                                       TRUE ~ 'other'),
           color_spotlight = factor(color_spotlight, levels = c('spotlight','teammates','other')))
  
  # visualization idea
  
  min_time <- floor(min(mens_10k_times$time_decimal,na.rm = T))
  max_time <- round(max(mens_10k_times$time_decimal,na.rm = T))+1
  
  athlete_times <- mens_10k_times |>
    filter(athlete == i)|>
    summarise(
      #ymin = min(time_decimal, na.rm = TRUE),
      #ymax = max(time_decimal, na.rm = TRUE),
      time_stat = median(time_decimal,na.rm = T)
    )
  
  athlete_pos <- mens_10k_times |> 
    filter(athlete == i) |> 
    pull(time_decimal)
  
  mens_10k_times |>
    ggplot(aes(x = 0, y = time_decimal)) + 
    geom_quasirandom(
      aes(fill = color_spotlight,
          color = color_spotlight,
          alpha = color_spotlight#,
          #size = color_spotlight
      ),
      shape = 21,
      stroke = 1,
      width = 0.4,
      method = 'smiley',
      size = 6
    ) +
    #geom_text(
    #  data = mens_10k_times |> filter(athlete == athlete_var),
    #  aes(x = -0.15, y = time_decimal + 0.5,  # Manually set x position
    #      label = glue('{athlete}:\n{decimal_to_time(time_decimal)}')),
    #  size = 4,
    #  fontface = "bold",
    #  family = 'noto',
    #  hjust = 1
    #) +
    geom_shadowtext(
      data = mens_10k_times |> filter(athlete == i),
      aes(label = glue('{athlete}:\n{decimal_to_time(time_decimal)}')),
      size = 4,
      fontface = "bold",
      family = 'noto',
      hjust = 1,
      bg.color = 'white',
      color = 'black'
    ) +
    coord_flip() +
    scale_y_reverse(
      limits = rev(c(min_time, max_time)),
      breaks = seq(min_time, max_time, by = 1.5),
      labels = function(x) {
        # FORMAT LABELS AS mm:ss
        mins <- floor(x)
        secs <- round((x - mins) * 60)
        sprintf("%d:%02d", mins, secs)
      }
    ) +
    scale_color_manual(
      values = c(
        "spotlight" = 'black',
        "teammates" = school_color_var_light,
        "other" = "gray70"
      ),
      guide = 'none'
    ) +
    scale_alpha_manual(
      values = c("spotlight" = 1, "teammates" = 0.7, "other" = 0.4)
    ) +
    #scale_size_manual(
    #  values = c("spotlight" = 5, "teammates" = 4, "top_n" = 3, "other" = 2.5)
    #) +
    scale_fill_manual(
      values = c(
        "spotlight" = school_color_var,
        "teammates" = school_color_var_light,
        "other" = "gray70"
      ),
      labels = c(
        "spotlight" = i,
        "teammates" = glue("{school_var}"),
        "other" = "Other runners"
      )
    ) +
    labs(
      x = '',
      y = '← Slower                                                     Faster →\n10K Race Time',
      #  title = glue("{data$athlete_var} ({data$school_var}) Performance Distribution")
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.caption = element_text(size = 8, hjust = 0, color = 'gray80'),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8,color = 'gray80',family = 'ibm'),
      axis.title.y = element_text(angle = 90,, hjust = 1, vjust = 1,color = 'gray80'),
      #axis.text.x = element_blank(),
      legend.title = element_blank(),
      plot.title.position = "plot", 
      plot.caption.position = 'plot',
      axis.line.y.left = element_blank(),
      axis.line.x.bottom = element_blank(),
      axis.ticks.y = element_blank(), 
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.major.x = element_line(color = 'gray90', linetype = 'dashed')
    )
  
  ggsave(glue('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/test/{i} dot plot test.png'),width = 6,height = 4,units = c('in'))
  
}

# density plot----

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