source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251118 Mens MW Regional Data Cleaning.R')

#   ---- runners who had the largest negative splits - first vs. second half ----

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
  as.data.frame()