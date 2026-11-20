# what are the winningest teams by conference in men's and women's college basketball since 1997?

if(!exists("setup_done")){
  library(tidyverse)
  library(janitor)
  library(glue)
  library(zoo)
  library(ggplot2)
  library(ggrepel)
  library(showtext)
  library(ggtext)
  library(gt)
  library(gtExtras)
  library(data.table)
  
  showtext_auto()
  showtext_opts(dpi = 300)
  
  font_add_google("Fira Code", "ibm")
  font_add_google("Noto Sans", "noto")
  
  setup_done <- T
  
}


# bring in the dataset  ----

full_cbb_dataset_mens <- read_rds('~/full_cbb_dataset_mens.rds')

source('~/20261105 School Slug Cleaning.R')

# variables for testing ----

conference_filter <- 'NEC'
rolling_win_var <- round(runif(1,2,5))

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
    paste0(tab, "x ", names(tab), "", suffix, collapse = ", ")
  })
}

# new rolling approach  ----

season_summary <- full_cbb_dataset_mens |>
  summarise(
    .by = c(season, school_name, conference),
    wins = sum(game_won, na.rm = T),
    losses = sum(game_lost, na.rm = T),
    across(c(ncaa_appearance, nit_appearance, cit_appearance, cbi_appearance, cbc_appearance, reg_season_champ, tourney_winner),
           ~max(.x,0,na.rm = T))
  ) |>
  rename_with(~ sub('_appearance$','',.x))

season_grid <- season_summary |>
  summarise(first = min(season), last = max(season), .by = school_name) |>
  reframe(season = first:last, .by = school_name)

season_full_schools <- season_grid |>
  left_join(season_summary, by = c("school_name", "season")) |>
  arrange(school_name, season) #|>
  #mutate(across(c(wins, losses, ncaa, nit, cit, cbi, cbc,
  #                reg_season_champ, tourney_winner), ~ replace_na(.x, 0)))


# rolling calculations for each window
roll_sum <- function(x, w) {
  cs <- cumsum(x)
  cs - dplyr::lag(cs, w, default = 0)           # valid because seasons are complete
}

add_rolling_totals <- function(df, w){
  df |>
    group_by(school_name) |>
    mutate(
      window_full = row_number() >= w,
      across(c(wins, losses, ncaa, nit, cit, cbi, cbc, reg_season_champ, tourney_winner),
             ~ if_else(window_full, roll_sum(.x, w), NA_real_),
             .names = "rolling_{.col}"),
      reg_conf_wins_rolling   = if_else(window_full, roll_conf_label(reg_season_champ, conference, w), NA_character_),
      tournament_wins_rolling = if_else(window_full, roll_conf_label(tourney_winner, conference, w), NA_character_)
    ) |>
    ungroup() |>
    mutate(window = w)
}

school_labels <- tibble(school_name = unique(season_full_schools$school_name)) |>
  mutate(school_label = short_name(school_name)) 

rolling_wins_cbb <- map(2:2, \(w) add_rolling_totals(season_full_schools, w)) |>
  list_rbind() |>
  filter(season >= 1997, season < 2027) |>
  mutate(rolling_win_ranked = dense_rank(desc(rolling_wins)),
         .by = c(window, season, conference)) |>
  left_join(school_labels, by = "school_name") |>
  mutate(
    post_season_appearances = pmap_chr(
      list(rolling_ncaa, rolling_nit, rolling_cit, rolling_cbi, rolling_cbc),
      \(ncaa, nit, cit, cbi, cbc) {
        x <- c(if (isTRUE(ncaa > 0)) glue::glue("March Madness ({ncaa}x)"),
               if (isTRUE(nit  > 0)) glue::glue("NIT ({nit}x)"),
               if (isTRUE(cit  > 0)) glue::glue("CIT ({cit}x)"),
               if (isTRUE(cbi  > 0)) glue::glue("CBI ({cbi}x)"),
               if (isTRUE(cbc  > 0)) glue::glue("CBC ({cbc}x)"))
        paste(x, collapse = ", ")
      })
  ) |>
  filter(!is.na(conference))


# what about a different way instead of Claude's fancy shit
roll_df_list <- list()
for(roll_n in 2:8){
  
  rolling_season_performance <- full_cbb_dataset_mens |>
    summarise(
      .by = c(season, school_name, conference),
      wins   = sum(game_won,  na.rm = T),
      losses = sum(game_lost, na.rm = T),
      across(c(ncaa_appearance, nit_appearance, cit_appearance, cbi_appearance,
               cbc_appearance, reg_season_champ, tourney_winner),
             ~ max(.x, 0, na.rm = T))
    ) |>
    rename_with(~ sub('_appearance$', '', .x)) |>
    filter(!is.na(conference)) |>                       # 1. drop non-D1 years BEFORE completing
    group_by(school_name) |>
    complete(season = full_seq(season, 1)) |>           #    only fills gaps inside the school's span
    arrange(school_name, season) |>
    mutate(
      played = as.integer(!is.na(wins)),                # 2. remember which seasons were real
      across(c(wins, losses, ncaa, nit, cit, cbi, cbc, reg_season_champ, tourney_winner),
             ~ replace_na(.x, 0)),                      #    skipped season = 0-0, no titles
      window_full = row_number() >= roll_n,
      rolling_seasons_played = rollapplyr(played, roll_n, sum, partial = T),
      across(c(wins, losses, ncaa, nit, cit, cbi, cbc), ~ rollapplyr(.x, roll_n, sum, partial = T),
             .names = 'rolling_{.col}'),
      rolling_win_pct = rolling_wins / (rolling_wins + rolling_losses),
      reg_conf_wins_rolling   = if_else(window_full, roll_conf_label(reg_season_champ, conference, roll_n), NA_character_),
      tournament_wins_rolling = if_else(window_full, roll_conf_label(tourney_winner,   conference, roll_n), NA_character_)
    ) |>
    ungroup() |>
    filter(season >= 1996, season < 2027, played == 1) |>   # hide the filler rows now
    mutate(
      rolling_ncaa_appearance = ifelse(rolling_ncaa > 0, paste0("March Madness (", rolling_ncaa, "x)"), NA),
      rolling_nit_appearance  = ifelse(rolling_nit  > 0, paste0("NIT (", rolling_nit, "x)"), NA),
      rolling_cit_appearance  = ifelse(rolling_cit  > 0, paste0("CIT (", rolling_cit, "x)"), NA),
      rolling_cbi_appearance  = ifelse(rolling_cbi  > 0, paste0("CBI (", rolling_cbi, "x)"), NA),
      rolling_cbc_appearance  = ifelse(rolling_cbc  > 0, paste0("CBC (", rolling_cbc, "x)"), NA)
    ) |>
    unite(post_season_appearances,
          c(rolling_ncaa_appearance, rolling_nit_appearance, rolling_cit_appearance, rolling_cbi_appearance, rolling_cbc_appearance),
          sep = ', ', na.rm = TRUE) |>
    select(school_name, season, conference, wins, losses, window_full, rolling_seasons_played,
           rolling_wins, rolling_losses, rolling_win_pct, #ends_with("_rolling"),
           reg_conf_wins_rolling, tournament_wins_rolling, post_season_appearances) |>
    mutate(window_rolling_win_var = roll_n)
  
  roll_df_list[[roll_n]] <- rolling_season_performance
  
}

rbindlist(roll_df_list) |>
  saveRDS('~/GitHub/Sports/College Basketball Winningest Teams by Conference/rolling winningest teams.rds')