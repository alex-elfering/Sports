# script and output visualizes the winningest men's college basketball teams since the 2000s
# data source: college basketball reference

library(tidyverse)
library(janitor)
library(glue)
library(lubridate)
library(zoo)
library(ggplot2)
library(ggrepel)
library(showtext)
library(ggtext)

showtext_auto()
showtext_opts(dpi = 300)

font_add_google("Fira Code", "ibm")
font_add_google("Noto Sans", "noto")

# data loading  -----------------------------------
mcbb_games <- read.csv('schedules_2012_2026.csv') |>
  clean_names() |>
  mutate(
    game_won = ifelse(tm>opp,1,0),
    game_lost = ifelse(tm<opp,1,0)
  )

# script provides clean slug names to add school name for provided slugs
source('20261105 School Slug Cleaning.R')


# data cleaning -----------------------------------
clean_mcbb <- mcbb_games |>
  unite(curr_record,c('w','l'),sep='-',na.rm = T) |>
  mutate(date = mdy(date)) |>
  inner_join(
    mcbb_slugnames, 
    by = c('slug' = 'school_slug')
  ) |>
  select(
    season,
    school_name = school_short,
    slug,
    g,
    date,
    type,
    opponent,
    loc = x,
    opp_conference = conf,
    result = x_1,
    tm,
    opp,
    ot,
    curr_record,
    game_won,
    game_lost
  ) |>
  mutate(opponent = str_squish(opponent)) |>   # <- trims ends and collapses internal runs of whitespace
  extract(
    opponent,
    into = c("opponent_name", "opponent_rank"),
    regex = "^(.*?)\\s*(?:\\((\\d+)\\))?$",
    remove = TRUE,    # drop the original so no untrimmed copy sticks around
    convert = TRUE
  ) |>
  mutate(opponent_name = trimws(opponent_name)) |>
  group_by(
    season,
    slug
  ) |>
  mutate(
    ncaa_appearance = max(ifelse(grepl('NCAA',type),1,0))
  ) |>
  ungroup() 

school_conferences <- clean_mcbb |>
  distinct(
    season, opponent_name, opp_conference
  ) |>
  rename(school = opponent_name, conference = opp_conference)

rolling_wins <- clean_mcbb |>
  filter(season >= 1990) |>
  left_join(school_conferences,
            by = c('school_name' = 'school',
                   'season' = 'season')) |>
  group_by(
    season, school_name, conference
  ) |>
  summarise(
    wins = sum(game_won,na.rm = T),
    losses = sum(game_lost,na.rm = T),
    .groups = 'drop'
  ) |>
  group_by(school_name) |>
  mutate(
    rolling_wins = rollapplyr(wins, width = 5, FUN = sum, partial = TRUE, na.rm = TRUE),
    rolling_losses = rollapplyr(losses, width = 5, FUN = sum, partial = T, na.rm = T),
  ) |>
  ungroup() |>
  group_by(season,conference) |>
  mutate(rolling_win_ranked = dense_rank(desc(rolling_wins))) |>
  ungroup() |> 
  filter(season >= 2001,
         season < 2027)

# plotting the data ----
conference_filter <- 'Big East'

last_season <- max(rolling_wins$season)

latest_winning_programs <- rolling_wins |>
  filter(conference == conference_filter
  ) |>
  filter(season == last_season) |>
  filter(dense_rank(desc(rolling_wins)) <= 3) |>
  pull(school_name)

big_east_df <- rolling_wins |>
  filter(
    conference == conference_filter
  ) |>
  # to account for some teams who might leave a conference and come back (lines will try to connect)
  group_by(
    school_name, conference 
  ) |>
  complete(season = full_seq(season,1)) |>
  ungroup()

bg_overall <- big_east_df |> filter(!school_name %in% latest_winning_programs)
bg_top_sch <- big_east_df |> filter(school_name %in% latest_winning_programs)
villanova <- big_east_df |>
  filter(
    school_name == 'Villanova'
  ) |>
  slice_max(rolling_wins, n = 1, with_ties = F)

team_colors <- c(
  'Connecticut'     = '#000e2f',
  'Creighton'       = '#005ca9',
  'Louisville'      = '#AD0000',
  "St. John's (NY)" = '#BA0C2F',
  'Syracuse'        = '#F76900'
)

team_labels <- c(
  'Connecticut'     = 'UConn',
  "St. John's (NY)" = "St. John's"
)

end_labels <- bg_top_sch |>
  filter(season == last_season) |>
  mutate(label = coalesce(unname(team_labels[school_name]), school_name))

# visualization ----

ggplot(
  mapping = aes(
    x = season,
    y = rolling_wins,
    group = school_name
  )
) +
  geom_line(
    data = bg_overall,
    color = 'gray85',
    linewidth = 0.4
  ) +
  geom_line(
    data = bg_top_sch, 
    color = 'white',
    linewidth = 2.2, 
    lineend = 'round') +
  geom_line(
    data = bg_top_sch, 
    aes(color = school_name),
    linewidth = 1,
    lineend = 'round') +
  geom_point(data = end_labels, color = 'white', size = 3.4) +
  geom_point(data = end_labels, aes(color = school_name), size = 2) +
  geom_text_repel(
    data = end_labels,
    aes(label = label, color = school_name),
    family = 'noto', fontface = 'bold', size = 3.5,
    hjust = 0, direction = 'y',
    xlim = c(last_season + 0.6, NA),
    segment.color = NA,
    show.legend = FALSE,
    bg.color = 'white', bg.r = 0.15
  ) +
  geom_text_repel(
    data = villanova,
    #aes(
    #  label = 'Villanova'
    #),
    label = 'Villanova',
    family = 'noto',
    size = 3.5,
    color = 'gray70',
    nudge_y = 6,
    segment.color = 'gray70',
    bg.color = 'white',
    bg.r = 0.15,
    fontface = 'bold'
  ) +
  geom_vline(
    xintercept = 2013,
    linetype = 'dashed',
    linewidth = 1,
    color = 'gray70'
  ) +
  #annotate("text", x = 2013, y = Inf, label = "New Big East", 
  #         vjust = 1.5, hjust = 0.5, fontface = "bold",family = 'noto') +
  annotate("text", x= 2012.7, y = Inf, label = 'Original Big East',
           vjust = 1.5, hjust = 1, fontface = 'bold', family = 'noto',
           size = 3.5, color = 'gray30') +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.05)),
    labels = function(x) {
      ifelse(x == max(x, na.rm = TRUE), paste0(x, '\nwins'), x)
    }
  ) + 
  scale_x_continuous(breaks = seq(2005, 2025, 5),
                     expand = expansion(mult = c(0.01, 0))) +
  labs(
    title = "UConn Remains Winningest MCBB in the Big East",
    subtitle = "Total program wins trailing five seasons 2001-2026, including post-season games.", 
    x = '',
    y = ''
  ) +
  coord_cartesian(clip = 'off') +
  scale_color_manual(values = team_colors, guide = 'none')  +
  theme(legend.position = "top",
        axis.text.x = element_text(size = 10, color = 'gray50', family = 'ibm'),
        axis.text.y = element_text(size = 10, color = 'gray50', family = 'ibm'),
        axis.title.y.left = element_markdown(size = 10, family = 'noto', angle = 0, vjust = 0.5),
        axis.title.x = element_text(size = 11, family = 'noto'),
        legend.title = element_blank(),
        legend.text = element_text(family = 'noto'),
        #plot.subtitle = ggtext::element_textbox_simple(lineheight = 1.2, margin = margin(t = 5, b = 15)),
        plot.title = element_text(family = 'noto',size = 16,face = 'bold'),
        plot.subtitle = element_textbox_simple(family = 'noto', size = 12, margin = margin(b = 15)),
        plot.title.position = "plot",
        axis.line = element_blank(), axis.ticks = element_blank(),
        panel.background = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = margin(t = 10, r = 60, b = 10, l = 40),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = 'gray92', linetype = 'solid'))

ggsave('big_east_winningest_teams.png', device = 'png',
       width = 7, height = 5, dpi = 300, bg = 'white')

