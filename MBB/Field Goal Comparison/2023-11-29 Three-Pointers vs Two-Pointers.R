# exploring three-point shooting tendencies

#roster ----
player_roster <- read.csv('C:/Users/alexe/OneDrive/Documents/Sports Analysis/MBB Returning Production/player roster.csv')

school_name_capitalized <- player_roster |>
  distinct(schl) |>
  mutate(school_name = case_when(schl == 'loyola-il' ~ 'Loyola (IL)',
                                 schl == 'miami-oh' ~ 'Miami (OH)',
                                 schl == 'illinois-chicago' ~ 'Illinois-Chicago',
                                 schl == 'iupui' ~ 'IUPUI',
                                 schl == 'nebraska-omaha' ~ 'Omaha',
                                 TRUE ~ tools::toTitleCase(gsub('-', ' ', schl))))

clean_roster <- player_roster |>
  left_join(school_name_capitalized) |>
  select(player,
         pos,
         season,
         school_name,
         class)

# player index  ----
player_index <- read.csv('C:/Users/alexe/OneDrive/Documents/Sports Analysis/MBB Returning Production/player game stats.csv')

clean_player_index <- player_index |>
  mutate(date = ymd(date),
         season = ifelse(month(date) >= 1 & month(date) <= 4, year(date)-1, year(date))) |>
  filter(schl != 'Schl') |>
  select(player,
         season,
         schl,
         gs,
         mp,
         ft,
         fta,
         fg,
         fga,
         x2p,
         x2pa,
         x3p,
         x3pa,
         pts)

shooting_stats <- clean_player_index |>
  select(-gs,
         -mp) |>
  pivot_longer(cols = -c('season', 'player', 'schl'),
               names_to = 'stat',
               values_to = 'value') |>
  group_by(season,
           player,
           schl,
           stat) |>
  mutate(value = as.numeric(value)) |>
  summarise_if(is.numeric, sum) |>
  ungroup()

playing_stats <- clean_player_index |>
  select(player,
         season,
         schl,
         gs,
         mp) |>
  group_by(player,
           season,
           schl) |>
  summarise(games = n(),
            gs = sum(as.numeric(gs)),
            mp = sum(as.numeric(mp))) |>
  ungroup()

# data export ----

write.csv(clean_roster, "C:/Users/alexe/OneDrive/Documents/GitHub/Sports/MBB/Field Goal Comparison/roster.csv")
write.csv(shooting_stats, "C:/Users/alexe/OneDrive/Documents/GitHub/Sports/MBB/Field Goal Comparison/shooting stats.csv")
write.csv(playing_stats, "C:/Users/alexe/OneDrive/Documents/GitHub/Sports/MBB/Field Goal Comparison/playing time.csv")