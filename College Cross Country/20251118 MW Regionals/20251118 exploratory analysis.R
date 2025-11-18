source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251118 Mens MW Regional Data Cleaning.R')

#   ----  overall spread of 10k times ----
mens_mw_clean |>
  filter(split == 10000) |>
  ggplot() + 
  geom_histogram(aes(time_decimal,group = athlete,fill = ifelse(school %in% c('Iowa State','Oklahoma State','Wichita State','Oklahoma'),school,NA)),color = 'white') +
  scale_x_reverse() +
  theme(legend.position = 'top')

# ----  what about the the most volatile parts of the race (the most changes in rank?)... how much volatility was there in the top 25+1?  ----
mens_mw_clean |>
  filter(!is.na(place_diff)) |>
  group_by(split) |>
  summarise(
    total_movement = sum(abs(place_diff)),
    average_movement = round(mean(abs(place_diff)),1),
    sd_movement = sd(place_diff),
    big_movers = sum(abs(place_diff) >= 10),
    pct_who_moved = mean(place_diff != 0) * 100
  )

mens_mw_clean |>
  #group_by(split) |>
  summarise(
    
    runners_top_5 = n_distinct(athlete[place == 1]),
    runners_top_5 = n_distinct(athlete[place <= 5]),
    runners_top_10 = n_distinct(athlete[place <= 10]),
    runners_top_15 = n_distinct(athlete[place <= 15]),
    runners_top_26 = n_distinct(athlete[place <= 26]),
    runners_top_50 = n_distinct(athlete[place <= 50]),
    runners_top_100 = n_distinct(athlete[place <= 100])
    
  )

mens_mw_clean |>
  group_by(school) |>
  summarise(
    
    runners_top_5 = n_distinct(athlete[place == 1]),
    runners_top_5 = n_distinct(athlete[place <= 5]),
    runners_top_10 = n_distinct(athlete[place <= 10]),
    runners_top_15 = n_distinct(athlete[place <= 15]),
    runners_top_26 = n_distinct(athlete[place <= 26]),
    runners_top_50 = n_distinct(athlete[place <= 50]),
    runners_top_100 = n_distinct(athlete[place <= 100])
    
  )

mens_mw_clean |>
  arrange(athlete, split) |>
  group_by(athlete) |>
  mutate(
    was_top_25 = !is.na(place) & place <= 25,
    was_top_25_prev = lag(was_top_25)
  ) |>
  ungroup() |>
  filter(!is.na(was_top_25_prev)) |>  # Remove first split for each athlete
  group_by(split) |>
  summarize(
    current_top_25 = sum(was_top_25, na.rm = TRUE),
    new_to_top_25 = sum(was_top_25 & !was_top_25_prev, na.rm = TRUE),
    dropped_from_top_25 = sum(!was_top_25 & was_top_25_prev, na.rm = TRUE),
    turnover_rate = (new_to_top_25 / current_top_25) * 100
  )

mens_mw_clean |>
  arrange(athlete, split) |>
  group_by(athlete) |>
  mutate(
    tier = case_when(
      place <= 5 ~ "Top 5",
      place <= 10 ~ "Top 10",
      place <= 25 ~ "Top 25",
      place <= 50 ~ "Top 50",
      TRUE ~ "51+"
    ),
    prev_tier = lag(tier),
    tier_change = tier != prev_tier
  ) |>
  ungroup() |>
  filter(!is.na(prev_tier)) |>
  group_by(split) |>
  summarize(
    pct_changed_tier = mean(tier_change, na.rm = TRUE) * 100,
    total_tier_changes = sum(tier_change, na.rm = TRUE)
  )

# ----  largest jumps in places by split
mens_mw_clean |>
  group_by(split) |>
  mutate(split_rank = dense_rank(split_diff_decimal)) |>
  filter(place_diff == max(place_diff,na.rm = T)) |>
  ungroup() |>
  arrange(split)

#   ----  runners who had the largest negative splits - first vs. second half ----

first_second_halves <- mens_mw_clean |>
  select(athlete,
         class,
         school,
         split,
         time_decimal) |>
  filter(split %in% c(5000,10000)) |>
  pivot_wider(names_from = split,
              values_from = time_decimal) |>
  clean_names() |>
  filter(!is.na(x10000)) |>
  mutate(second_5k_decimal = x10000-x5000) |>
  rename(first_5k_decimal = x5000,
         final_time_decimal = x10000) |>
  mutate(plus_minus = second_5k_decimal-first_5k_decimal) |>
  select(athlete,
         class,
         school,
         first_5k_decimal,
         second_5k_decimal,
         plus_minus,
         final_time_decimal) |>
  arrange(plus_minus) |>
  mutate(first_5k_time = decimal_to_time(first_5k_decimal),
         second_5k_time = decimal_to_time(second_5k_decimal),
         plus_minus_time = decimal_to_time(plus_minus),
         final_time = decimal_to_time(final_time_decimal))

median_split <- median(first_second_halves$plus_minus)
average_split <- mean(first_second_halves$plus_minus)

first_second_split_halves_df <- first_second_halves |>
  filter(plus_minus < average_split) |>
  filter(dense_rank(plus_minus) <= 26) |>
  as.data.frame() |>
  select(-contains('decimal'),
         -plus_minus)