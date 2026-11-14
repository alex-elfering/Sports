# script and output visualizes the winningest men's college basketball teams since the 2000s
# data source: college basketball reference

if(!exists("setup_done")){
  library(tidyverse)
  library(janitor)
  library(glue)
  library(lubridate)
  library(zoo)
  library(ggplot2)
  library(ggrepel)
  library(showtext)
  library(ggtext)
  library(gt)
  library(gtExtras)
  
  showtext_auto()
  showtext_opts(dpi = 300)
  
  font_add_google("Fira Code", "ibm")
  font_add_google("Noto Sans", "noto")
  
  setup_done <- T
  
}

documents_loc <- "~/"
proj_loc <- '~/GitHub/Sports/MCBB Winningest Big East Teams'

# functions ----
roll_conf_label <- function(flag, conf, width, suffix = "") {
  # needed to identify teams that either won regular season championships or conference tournament championships
  # they might win one conference one season, then the next season, move a conference and win there, too
  
  # flag    - 0/1 identifying if the team won the title that season (reg season or tourney)
  # conf    - what conference the team played in
  # width   - number of seasons counted in the rolling window 
  # suffix  - text added after the conference name 
  
  # loop over every season
  purrr::map_chr(seq_along(flag), function(i) {
    
    # row positions in this window
    idx <- seq.int(max(1, i - width + 1), i)
    # keep rows where a title is won
    hit <- idx[flag[idx] > 0]
    # if there are no titles, then nothing to label
    if (length(hit) == 0) return(NA_character_)
    # conferences that the team won a title in, starting with the most recent title
    confs <- rev(conf[hit])
    # count titles per conference; the factor keeps the newest-first order
    tab <- table(factor(confs, levels = unique(confs)))
    # "1x SEC winner, 2x Big 12 winner"
    paste0(tab, "x ", names(tab), " ", suffix, collapse = ", ")
  })
}

# nicknames vector  ----
school_short_names <- c(
  "Brigham Young"                   = "BYU",
  "Southern California"             = "USC",
  "Southern Methodist"              = "SMU",
  "Texas Christian"                 = "TCU",
  "Louisiana State"                 = "LSU",
  "Virginia Commonwealth"           = "VCU",
  "Virginia Military Institute"     = "VMI",
  "Nevada–Las Vegas"                = "UNLV",
  "Connecticut"                     = "UConn",
  "Massachusetts"                   = "UMass",
  "Massachusetts–Lowell"            = "UMass Lowell",
  "Pennsylvania"                    = "Penn",
  "Pittsburgh"                      = "Pitt",
  "Mississippi"                     = "Ole Miss",
  "Southern Mississippi"            = "Southern Miss",
  "California"                      = "Cal",
  "George Washington"               = "GW",
  "William & Mary"                  = "W&M",
  "Miami (FL)"                      = "Miami",
  "St. John's (NY)"                 = "St. John's",
  "Albany (NY)"                     = "Albany",
  "Loyola (IL)"                     = "Loyola Chicago",
  "Loyola (MD)"                     = "Loyola Maryland",
  "Loyola Marymount"                = "LMU",
  "Saint Francis (PA)"              = "Saint Francis",
  "Illinois–Chicago"                = "UIC",
  "Marylandâ€“Baltimore County"     = "UMBC",
  "Maryland–Baltimore County"       = "UMBC",
  "Maryland–Eastern Shore"          = "UMES",
  "Southern Illinois–Edwardsville"  = "SIUE",
  "Tennessee–Martin"                = "UT Martin",
  "Louisiana–Monroe"                = "UL Monroe",
  "Arkansas–Pine Bluff"             = "Arkansas-Pine Bluff",
  "Texas A&M–Corpus Christi"        = "A&M-Corpus Christi",
  "Texas–Rio Grande Valley"         = "UTRGV",
  "Florida International"           = "FIU",
  "Florida Atlantic"                = "FAU",
  "Florida Gulf Coast"              = "FGCU",
  "Long Island University"          = "LIU",
  "Stephen F. Austin"               = "SFA",
  "Southeast Missouri State"        = "SEMO",
  "Southeastern Louisiana"          = "SE Louisiana",
  "Central Connecticut State"       = "Central Conn.",
  "Northern Kentucky"               = "NKU",
  "Western Kentucky"                = "WKU",
  "Eastern Kentucky"                = "EKU",
  "North Carolina A&T"              = "NC A&T",
  "North Carolina Central"          = "NC Central",
  "Mississippi Valley State"        = "Miss. Valley St.",
  "Prairie View A&M"                = "Prairie View",
  "Cal State Bakersfield"           = "CSU Bakersfield",
  "Cal State Fullerton"             = "CSU Fullerton",
  "Cal State Los Angeles"           = "CSU LA",
  "Cal State Northridge"            = "CSUN",
  "Bethune-Cookman"                 = 'B-CU'
)

# data loading  -----------------------------------
mcbb_games <- read.csv(file.path(documents_loc, 'schedules_2012_2026.csv')) |>
  clean_names() |>
  mutate(
    game_won = ifelse(tm>opp,1,0),
    game_lost = ifelse(tm<opp,1,0)
  )

# script provides clean slug names to add school name for provided slugs
source(file.path(documents_loc, '20261105 School Slug Cleaning.R'))

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
    ncaa_appearance = max(ifelse(grepl('NCAA',type),1,0)),
    cit_appearance = max(ifelse(grepl('CIT',type),1,0)),
    cbi_appearance = max(ifelse(grepl('CBI',type),1,0)),
    nit_appearance = max(ifelse(grepl('NIT',type),1,0)),
    cbc_appearance = max(ifelse(grepl('CBC',type),1,0)),
  ) |>
  ungroup() 

school_conferences <- clean_mcbb |>
  distinct(
    season, opponent_name, opp_conference
  ) |>
  rename(school = opponent_name, conference = opp_conference)

tournament_winner_list <- clean_mcbb |>
  left_join(school_conferences,
            by = c('school_name' = 'school',
                   'season' = 'season')) |>
  filter(
    type == 'CTOURN'
  ) |>
  filter(result == 'W') |>
  group_by(
    season,
    conference
  ) |>
  slice_max(date) |>
  ungroup() |>
  select(
    season,
    school_name,
    conference
  ) |>
  distinct() |>
  mutate(tourney_winner = 1)

regular_conf_champions <- clean_mcbb |>
  left_join(school_conferences,
            by = c('school_name' = 'school',
                   'season' = 'season')) |>
  filter(
    type == 'REG',
    conference == opp_conference
  ) |>
  group_by(
    season,
    school_name,
    conference
  ) |>
  summarise(conf_w = sum(game_won,na.rm = T),
            conf_l = sum(game_lost,na.rm = T),
            .groups = 'drop') |>
  group_by(
    season,
    conference
  ) |>
  mutate(pct_conf = conf_w/(conf_w+conf_l)) |>
  filter(pct_conf == max(pct_conf)) |>
  ungroup() |>
  select(
    -conf_w,
    -conf_l,
    -pct_conf
  ) |>
  mutate(
    reg_season_champ = 1
  )

rolling_win_var <- round(runif(1,2,5))

rolling_wins <- clean_mcbb |>
  arrange(season,
          school_name) |>
  left_join(school_conferences,
            by = c('school_name' = 'school',
                   'season' = 'season')) |>
  left_join(tournament_winner_list,
            by = c('season' = 'season',
                   'conference' = 'conference',
                   'school_name' = 'school_name')) |>
  left_join(regular_conf_champions,
            by = c('season' = 'season',
                   'conference' = 'conference',
                   'school_name' = 'school_name')) |>
  #filter(school_name == 'Harvard',
  #       season >= 2019) |>
  group_by(
    season, school_name, conference
  ) |>
  summarise(
    wins = sum(game_won,na.rm = T),
    losses = sum(game_lost,na.rm = T),
    ncaa = max(ncaa_appearance,na.rm = T),
    nit = max(nit_appearance,na.rm = T),
    cit = max(cit_appearance,na.rm = T),
    cbi = max(cbi_appearance,na.rm = T),
    cbc = max(cbc_appearance,na.rm = T),
    reg_season_champ = max(reg_season_champ,na.rm = T),
    tourney_winner = max(tourney_winner,na.rm = T),
    .groups = 'drop'
  ) |>
  #group_by(school_name, conference) |>
  #complete(season = full_seq(season,1)) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -Inf, 0))) |>
  #ungroup() |>
  group_by(school_name) |>
  arrange(school_name, season) |>
  group_by(school_name) |>
  mutate(
    rolling_wins   = rollapplyr(wins,   width = rolling_win_var, FUN = sum, partial = TRUE, na.rm = TRUE),
    rolling_losses = rollapplyr(losses, width = rolling_win_var, FUN = sum, partial = TRUE, na.rm = TRUE),
    rolling_ncaa   = rollapplyr(ncaa,   width = rolling_win_var, FUN = sum, partial = TRUE, na.rm = TRUE),
    rolling_nit    = rollapplyr(nit,    width = rolling_win_var, FUN = sum, partial = TRUE, na.rm = TRUE),
    rolling_cit    = rollapplyr(cit,    width = rolling_win_var, FUN = sum, partial = TRUE, na.rm = TRUE),
    rolling_cbi    = rollapplyr(cbi,    width = rolling_win_var, FUN = sum, partial = TRUE, na.rm = TRUE),
    rolling_cbc    = rollapplyr(cbc,    width = rolling_win_var, FUN = sum, partial = TRUE, na.rm = TRUE),
    rolling_reg_conf_champ = rollapplyr(reg_season_champ, width = rolling_win_var, FUN = sum, partial = TRUE, na.rm = TRUE),
    rolling_tourney_winner = rollapplyr(tourney_winner,   width = rolling_win_var, FUN = sum, partial = TRUE, na.rm = TRUE),
    
    # conference-aware labels
    reg_conf_wins_rolling   = roll_conf_label(reg_season_champ, conference, rolling_win_var),
    tournament_wins_rolling = roll_conf_label(tourney_winner,   conference, rolling_win_var)
  ) |>
  mutate(
    rolling_ncaa_appearance = ifelse(rolling_ncaa > 0, paste0("March Madness (", rolling_ncaa, "x)"), NA),
    rolling_nit_appearance  = ifelse(rolling_nit  > 0, paste0("NIT (", rolling_nit, "x)"), NA),
    rolling_cit_appearance  = ifelse(rolling_cit  > 0, paste0("CIT (", rolling_cit, "x)"), NA),
    rolling_cbi_appearance  = ifelse(rolling_cbi  > 0, paste0("CBI (", rolling_cbi, "x)"), NA),
    rolling_cbc_appearance  = ifelse(rolling_cbc  > 0, paste0("CBC (", rolling_cbc, "x)"), NA)
  ) |>
  unite(post_season_appearances,
        c(rolling_ncaa_appearance, rolling_nit_appearance, rolling_cit_appearance,
          rolling_cbi_appearance, rolling_cbc_appearance),
        sep = ', ', na.rm = TRUE) |>
  ungroup() |>
  group_by(season,conference) |>
  mutate(rolling_win_ranked = dense_rank(desc(rolling_wins))) |>
  ungroup() |> 
  filter(season >= 1997,
         season < 2027) |>
  mutate(school_label = short_name(school_name))

# ----  if the user selects different conferences?  ----

conference_filter <- rolling_wins |>
  filter(season == max(season)) |>
  distinct(conference) |>
  sample_n(1) |>
  pull(conference)

last_season <- max(rolling_wins$season)

rolling_conference_filter <- rolling_wins |>
  filter(
    conference == conference_filter
  ) |>
  # to account for some teams who might leave a conference and come back (lines will try to connect)
  group_by(
    school_name, conference 
  ) |>
  complete(season = full_seq(season,1)) |>
  ungroup()

latest_winning_programs <- rolling_conference_filter |>
  filter(season == last_season) |>
  filter(dense_rank(desc(rolling_wins)) <= 3) |>
  pull(school_name)

# did another program peak in the visualization that isn't among the top 3 winningest programs?
other_peak_schools <- rolling_conference_filter |>
  slice_max(rolling_wins, with_ties = F) |>
  filter(!school_name %in% latest_winning_programs) |>
  select(school_name,
         season,
         rolling_wins,
         school_label)

top_conference <- rolling_conference_filter |>
  filter(school_name %in% latest_winning_programs)

oth_conference <- rolling_conference_filter |>
  filter(!school_name %in% latest_winning_programs)

oth_peak_schoo <- rolling_conference_filter |>
  filter(school_name %in% other_peak_schools$school_name)

end_labels <- top_conference |>
  filter(season == last_season) |>
  mutate(label = school_name)

# visualization testing ----

conference_trending <- ggplot(  
  mapping = aes(
    x = season,
    y = rolling_wins,
    group = school_name
  )) + 
  geom_line(
    data = oth_conference,
    mapping = aes(
      x = season,
      y = rolling_wins,
      group = school_name
    ),
    linewidth = 0.5,
    color = 'gray70',
    alpha = 0.3
  ) +
  geom_line(
    data = oth_peak_schoo,
    mapping = aes(
      x = season,
      y = rolling_wins,
      group = school_name
    ),
    linewidth = 2.2,
    color = 'white'
  ) +
  geom_line(
    data = oth_peak_schoo,
    mapping = aes(
      x = season,
      y = rolling_wins,
      group = school_name
    ),
    linewidth = 1,
    color = 'gray70',
    #alpha = 0.4
  ) +
  geom_line(
    data = oth_conference,
    mapping = aes(
      x = season,
      y = rolling_wins,
      group = school_name
    ),
    linewidth = 0.5,
    color = 'gray70',
    alpha = 0.4
  ) +
  geom_line(
    data = top_conference,
    mapping = aes(
      x = season,
      y = rolling_wins,
      group = school_name
    ),
    linewidth = 2.2,
    color = 'white',
    lineend = 'round'
  ) +
  geom_line(
    data = top_conference,
    mapping = aes(
      x = season,
      y = rolling_wins,
      group = school_name,
      color = school_name
    ),
    linewidth = 1,
    lineend = 'round'
  ) +
  geom_point(data = end_labels, color = 'white', size = 3.4) +
  geom_point(data = end_labels, aes(color = school_name), size = 2) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.05)),
    labels = function(x) {
      ifelse(x == max(x, na.rm = TRUE), paste0(x, '\nwins'), x)
    }
  ) + 
  scale_x_continuous(
    breaks = seq(1997, 2025, 4),
    expand = expansion(add = c(0.3, 5))
  ) +
  geom_text_repel(
    data = end_labels,
    aes(label = school_label, color = school_name),
    family = 'noto', fontface = 'bold', size = 3.5,
    hjust = 0, 
    direction = 'y',
    nudge_x = 0.6,
    xlim = c(last_season + 0.6, NA),
    segment.color = NA,
    show.legend = FALSE,
    bg.color = 'white', bg.r = 0.15
  ) +
  geom_text_repel(
    data = other_peak_schools,
    aes(
      label = school_label
    ),
    #label = school_name,
    family = 'noto',
    size = 3.5,
    color = 'gray70',
    nudge_y = 6,
    segment.color = 'gray70',
    bg.color = 'white',
    bg.r = 0.15,
    fontface = 'bold'
  ) +
  labs(
    title = glue("Winningest MCBB Teams in the {conference_filter} Conference"),
    subtitle = glue("Total program wins trailing {rolling_win_var} seasons 1997-2026, including post-season games."), 
    caption = 'Source: College-Basketball Reference; Data Visualization created by Alex Elfering',
    x = '',
    y = ''
  ) +
  coord_cartesian(clip = 'off') +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10, color = 'gray50', family = 'ibm'),
        axis.text.y = element_text(size = 10, color = 'gray50', family = 'ibm'),
        axis.title.y.left = element_markdown(size = 10, family = 'noto', angle = 0, vjust = 0.5),
        axis.title.x = element_text(size = 11, family = 'noto'),
        legend.title = element_blank(),
        legend.text = element_text(family = 'noto'),
        #plot.subtitle = ggtext::element_textbox_simple(lineheight = 1.2, margin = margin(t = 5, b = 15)),
        plot.title = element_text(family = 'noto', size = 15, face = 'bold',margin = margin(b = 1)),
        plot.subtitle = element_text(family = 'noto', size = 12,margin = margin(t = 1, b = 15)),
        plot.caption = element_text(family = 'noto', size = 8, color = 'gray40',hjust = 0, margin = margin(t = 1)),
        plot.caption.position = 'plot',
        #plot.margin = margin(r = 100,t=1,b=1,l=1),
        plot.margin = margin(r = 1, t = 1, b = 1, l = 1),
        plot.title.position = "plot",
        axis.line = element_blank(), axis.ticks = element_blank(),
        panel.background = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = 'gray92', linetype = 'solid'))

ggsave(plot = conference_trending,file.path(documents_loc, 'test_winningest_teams.png'), device = 'png', width = 7, height = 5, dpi = 300, bg = 'white')

# table ----

rolling_table_detail <- rolling_conference_filter |>
  filter(season == max(season)) |>
  arrange(rolling_win_ranked) |>
  unite(total_record, c(rolling_wins, rolling_losses), sep='-') |>
  mutate(
    reg_conf_wins_rolling = ifelse(is.na(reg_conf_wins_rolling),"",reg_conf_wins_rolling),
    tournament_wins_rolling = ifelse(is.na(tournament_wins_rolling),"",tournament_wins_rolling)
  ) |>
  select(
    #season,
    school_name,
    total_record,
    reg_conf_wins_rolling,
    tournament_wins_rolling,
    post_season_appearances
  )

rolling_table_detail |>
  gt() |>
  # column labels
  cols_label(
    school_name = 'School',
    total_record = 'Total Record',
    reg_conf_wins_rolling = 'Regular Season Championships',
    tournament_wins_rolling = 'Tournament Championships',
    post_season_appearances = 'Post-Season Tournament Appearances'
  ) |>
  # alignment of columns
  cols_align(
    align = 'center',
    columns = c(total_record, reg_conf_wins_rolling, tournament_wins_rolling)
  ) |>
  cols_align(
    align = 'right',
    columns = c(school_name)
  ) |>
  # border formatting
  gt_add_divider(
    columns = c(total_record, tournament_wins_rolling),
    weight = px(1),
    color = 'gray50'
  ) |>
  # fonts
  tab_style(
    style = cell_text(
      font = google_font('Fira Code'),
      color = 'gray30'
    ),
    locations = cells_body(columns = c(total_record))
  ) |>
  tab_style(
    style = cell_text(
      font = google_font('Noto Sans'),
    ),
    locations = cells_body(columns = -c(total_record))
  ) |>
  # title formatting
  tab_options(
    table.border.top.style    = "none",  
    table.border.bottom.style = "none"   
  ) |>
  opt_row_striping() |>
  tab_options(
    table.font.size = px(14),
    heading.align = "left",
    column_labels.font.weight = "bold",
    table.border.top.color = "black",
    table.border.bottom.color = "black"
  )
