
if(!exists("setup_done")){
  library(ggplot2)
  library(ggrepel)
  library(showtext)
  library(ggtext)
  library(gt)
  library(gtExtras)
  library(gghighlight)
  library(tidyverse)
  
  showtext_auto()
  showtext_opts(dpi = 300)
  
  font_add_google("Fira Code", "ibm")
  font_add_google("Noto Sans", "noto")
  
  setup_done <- T
  
}

cbb_rolling_win_teams <- read_rds('~/GitHub/Sports/College Basketball Winningest Teams by Conference/rolling winningest teams.rds')

# variables to tesr right now ----
window_rolling <- 3

test_cbb_rolling <- cbb_rolling_win_teams |>
  filter(season == max(season)) |>
  filter(window_rolling_win_var == window_rolling)

# perhaps the user wants to look at all-time winningest programs outside of conference
#conference_selected <- 'All'
conference_selected <- 'A-10'

top_n_view <- 6

# functions for visualizations  ----

theme_cbb <- function(){
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9, color = 'gray50', family = 'ibm'),
        axis.text.y = element_text(size = 9, color = 'gray50', family = 'ibm'),
        axis.title.y.left = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        plot.title = element_text(family = 'noto', size = 15, face = 'bold',margin = margin(b = 1)),
        plot.subtitle = element_text(family = 'noto', size = 12,margin = margin(t = 1, b = 15)),
        plot.caption = element_text(family = 'noto', size = 8, color = 'gray40',hjust = 0, margin = margin(t = 1)),
        plot.caption.position = 'plot',
        plot.margin = margin(r = 1, t = 1, b = 1, l = 1),
        plot.title.position = "plot",
        axis.line = element_blank(), axis.ticks = element_blank(),
        panel.background = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = 'gray92', linetype = 'solid'),
        strip.background = element_blank(),
        strip.text = element_text(family = 'noto', size = 10, face = 'bold')
  )
}

# prepare dataframe ----


if(conference_selected == 'All'){
  
  top_schools <- test_cbb_rolling |>
    arrange(desc(rolling_win_pct)) |>
    filter(dense_rank(desc(rolling_win_pct)) <= top_n_view) |>
    pull(school_name)
  
  top_school_stats <- cbb_rolling_win_teams |>
    filter(school_name %in% top_schools) |>
    filter(window_rolling_win_var == window_rolling) |>
    arrange(school_name,
            desc(season)) |>
    group_by(school_name) |>
    filter(row_number() <= window_rolling) |>
    ungroup() |>
    arrange(school_name) |>
    summarise(
      .by = c('school_name'),
      total_wins = sum(wins),
      total_losses = sum(losses)
    ) |>
    unite(record, c('total_wins','total_losses'), sep ='-')
  
  filtered_dataframe_visual <- cbb_rolling_win_teams |>
    filter(window_rolling_win_var == window_rolling) |>
    filter(school_name %in% top_schools) |>
    mutate(school_name = factor(school_name, levels = top_schools)) #|>
  
  last_season <- max(test_cbb_rolling$season)
  
  end_labels <- test_cbb_rolling |>
    filter(school_name %in% top_schools) |>
    mutate(school_name = factor(school_name, levels = top_schools)) |>
    select(
      season,
      school_name,
      rolling_win_pct
    ) 
  
  ggplot(
    mapping = aes(
      x = season,
      y = rolling_win_pct,
      group = school_name
    )
  ) + 
    geom_line(filtered_dataframe_visual |> filter(rolling_seasons_played < window_rolling),
              mapping = aes( x = season,
                             y = rolling_win_pct,
                             group = school_name),
              linetype = 'dashed',
              color = 'steelblue',
              lineend = 'round',
              linewidth = 1) +
    geom_line(filtered_dataframe_visual |> filter(rolling_seasons_played >= window_rolling),
              mapping = aes( x = season,
                             y = rolling_win_pct,
                             group = school_name),
              linetype = 'solid',
              color = 'steelblue',
              lineend = 'round',
              linewidth = 1) +
    gghighlight(max(rolling_wins) > 0,
                use_direct_label = F,
                unhighlighted_params = list(alpha = 0.3, linewidth = 0.75)) +
    geom_text_repel(
      data = end_labels,
      aes(label = paste0(round(rolling_win_pct*100,1), "%") ),
      color = 'steelblue',
      family = 'noto', fontface = 'bold', size = 3.5,
      hjust = 0, 
      direction = 'y',
      nudge_x = 0.6,
      xlim = c(last_season + 0.6, NA),
      segment.color = NA,
      show.legend = FALSE,
      bg.color = 'white', bg.r = 0.15
    ) +
    labs(
      x = '',
      y = ''
    ) +
    facet_wrap(~school_name,
               scales = 'free_x',ncol = 3) +
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.05)),
      labels = function(x) {
        ifelse(x == max(x, na.rm = TRUE), paste0( paste0(x*100,'%'), '\ngames won'), paste0( paste0(x*100,'%')))
      }
    ) + 
    coord_cartesian(clip = 'off') +
    theme_cbb()
  
}else if(conference_selected != 'All'){
  
  top_schools <- test_cbb_rolling |>
    arrange(desc(rolling_win_pct)) |>
    filter( conference == conference_selected ) |>
    pull(school_name)
  
  top_school_stats <- cbb_rolling_win_teams |>
    filter(school_name %in% top_schools) |>
    filter(window_rolling_win_var == window_rolling) |>
    arrange(school_name,
            desc(season)) |>
    group_by(school_name) |>
    filter(row_number() <= window_rolling) |>
    ungroup() |>
    arrange(school_name) |>
    summarise(
      .by = c('school_name'),
      total_wins = sum(wins),
      total_losses = sum(losses)
    ) |>
    unite(record, c('total_wins','total_losses'), sep ='-')
  
  filtered_dataframe_visual <- cbb_rolling_win_teams |>
    filter(window_rolling_win_var == window_rolling) |>
    filter(school_name %in% top_schools) |>
    mutate(school_name = factor(school_name, levels = top_schools)) #|>
  
  last_season <- max(test_cbb_rolling$season)
  
  end_labels <- test_cbb_rolling |>
    filter(school_name %in% top_schools) |>
    mutate(school_name = factor(school_name, levels = top_schools)) |>
    select(
      season,
      school_name,
      rolling_win_pct
    ) 
  
  ggplot(
    mapping = aes(
      x = season,
      y = rolling_win_pct,
      group = school_name
    )
  ) + 
    geom_line(filtered_dataframe_visual |> filter(rolling_seasons_played < window_rolling),
              mapping = aes( x = season,
                             y = rolling_win_pct,
                             group = school_name),
              linetype = 'dashed',
              color = 'steelblue',
              lineend = 'round',
              linewidth = 1) +
    geom_line(filtered_dataframe_visual |> filter(rolling_seasons_played >= window_rolling),
              mapping = aes( x = season,
                             y = rolling_win_pct,
                             group = school_name),
              linetype = 'solid',
              color = 'steelblue',
              lineend = 'round',
              linewidth = 1) +
    gghighlight(max(rolling_wins) > 0,
                use_direct_label = F,
                unhighlighted_params = list(alpha = 0.3, linewidth = 0.75)) +
    geom_text_repel(
      data = end_labels,
      aes(label = paste0(round(rolling_win_pct*100,1), "%") ),
      color = 'steelblue',
      family = 'noto', fontface = 'bold', size = 3.5,
      hjust = 0, 
      direction = 'y',
      nudge_x = 0.6,
      xlim = c(last_season + 0.6, NA),
      segment.color = NA,
      show.legend = FALSE,
      bg.color = 'white', bg.r = 0.15
    ) +
    labs(
      x = '',
      y = ''
    ) +
    facet_wrap(~school_name,
               #scales = 'free_x',
               ncol = 3) +
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.05)),
      labels = function(x) {
        ifelse(x == max(x, na.rm = TRUE), paste0( paste0(x*100,'%'), '\ngames won'), paste0( paste0(x*100,'%')))
      }
    ) + 
    coord_cartesian(clip = 'off') +
    theme_cbb()
  
  
}


ggsave('~/test_winningest_teams.png', device = 'png', width = 10, height = 10, dpi = 300, bg = 'white')
