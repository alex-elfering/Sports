
if(!exists("setup_done")){
  library(ggplot2)
  library(ggrepel)
  library(showtext)
  library(ggtext)
  library(gt)
  library(gtExtras)
  library(gghighlight)
  
  showtext_auto()
  showtext_opts(dpi = 300)
  
  font_add_google("Fira Code", "ibm")
  font_add_google("Noto Sans", "noto")
  
  setup_done <- T
  
}

cbb_rolling_win_teams <- read_rds('~/GitHub/Sports/College Basketball Winningest Teams by Conference/rolling winningest teams.rds')

# variables to tesr right now ----
window_rolling <- 8

test_cbb_rolling <- cbb_rolling_win_teams |>
  filter(season == max(season)) |>
  filter(window_rolling_win_var == window_rolling)

# perhaps the user wants to look at all-time winningest programs outside of conference
conference_selected <- 'All'
#conference_selected <- 'Big Ten'

top_n_view <- 5

# prepare dataframe ----

if(conference_selected == 'All'){
  
  top_schools <- test_cbb_rolling |>
    arrange(desc(rolling_wins)) |>
    filter(dense_rank(desc(rolling_wins)) <= top_n_view) |>
    pull(school_name)
  
}else if (conference_selected != 'All'){
  
  top_schools <- test_cbb_rolling |>
    filter(conference == conference_selected) |>
    arrange(desc(rolling_wins)) |>
    filter(dense_rank(desc(rolling_wins)) <= top_n_view)
  
}


cbb_rolling_win_teams |>
  filter(window_rolling_win_var == window_rolling) |>
  filter(school_name %in% top_schools) |>
  mutate(school_name = factor(school_name, levels = top_schools)) |>
  ggplot() + 
  geom_line(mapping = aes( x = season,
                           y = rolling_wins,
                           group = school_name),
            color = 'steelblue',
            lineend = 'round',
            linewidth = 2) +
  gghighlight(max(rolling_wins) > 0,
              use_direct_label = F) +
  labs(
    x = '',
    y = ''
  ) +
  facet_wrap(~school_name) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.05)),
    labels = function(x) {
      ifelse(x == max(x, na.rm = TRUE), paste0(x, '\nwins'), x)
    }
  ) + 
  coord_cartesian(clip = 'off') +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10, color = 'gray50', family = 'ibm'),
        axis.text.y = element_text(size = 10, color = 'gray50', family = 'ibm'),
        axis.title.y.left = element_markdown(size = 10, family = 'noto', angle = 0, vjust = 0.5),
        axis.title.x = element_text(size = 11, family = 'noto'),
        legend.title = element_blank(),
        legend.text = element_text(family = 'noto'),
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
        # facet_wrap formatting
        strip.background = element_blank(),
        strip.text = element_text(family = 'noto', size = 10,margin = margin(t = 1, b = 15))
        )