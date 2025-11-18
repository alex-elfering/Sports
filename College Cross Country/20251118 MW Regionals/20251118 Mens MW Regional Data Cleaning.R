
####  NCAA Midwest Regional Cross Country 2025

# ----  libraries ----
library(ggplot2)
library(tidyverse)
library(janitor)
library(readxl)

# ----  data pull  ----
mens_midwest_regionals_25 <- read_xlsx("C:/Users/alexe/OneDrive/Desktop/xc midwest results 2025.xlsx")

# ----  functions ----
time_to_decimal <- function(time_string) {
  parts <- as.numeric(strsplit(time_string, ":")[[1]])
  minutes <- parts[1]
  seconds <- parts[2]
  return(minutes + seconds/60)
}

decimal_to_time <- function(decimal_mins) {
  minutes <- floor(decimal_mins)
  seconds <- (decimal_mins - minutes) * 60
  return(sprintf("%d:%04.1f", minutes, seconds))
}


# ----  data cleaning ----
mens_mw_clean <- mens_midwest_regionals_25 |>
  clean_names() |>
  # Pivot times
  pivot_longer(
    cols = matches("^x\\d+m$"),  # Matches columns like x2000m, x3300m, etc.
    names_to = "split",
    values_to = "time"
  ) |>
  # Pivot places
  pivot_longer(
    cols = starts_with("place_"),
    names_to = "place_split",
    values_to = "place"
  ) |>
  # Extract the distance from both split columns
  mutate(
    split_distance = str_extract(split, "\\d+"),
    place_distance = str_extract(place_split, "\\d+")
  ) |>
  # Keep only matching split-place pairs
  filter(split_distance == place_distance) |>
  # Clean up
  select(-split_distance, -place_distance, -place_split) |>
  # Format times
  mutate(
    time_formatted = format(time, "%M:%OS1"),
    time_formatted = sub("^0", "", time_formatted),
    time_decimal = sapply(time_formatted, time_to_decimal),
    split = as.numeric(gsub('[xm]','',split))
  ) |>
  group_by(school, athlete) |>
  arrange(school, athlete, split) |>
  mutate(
    split_diff_decimal = time_decimal - lag(time_decimal),
    split_diff = ifelse(!is.na(split_diff_decimal), 
                        decimal_to_time(split_diff_decimal),
                        NA_character_),
    place_diff = lag(place)-place
  ) |>
  ungroup() |>
  select(-time)

# ----  exploratory ----

# overall spread of 10k times
mens_mw_clean |>
  #filter(split == 2000) |>
  ggplot() + 
  geom_histogram(aes(time_decimal)) +
  scale_x_reverse()

# spread by split
mens_mw_clean |>
  #filter(split == 2000) |>
  ggplot() + 
  geom_histogram(aes(time_decimal)) +
  scale_x_reverse() +
  facet_wrap(~split,
             scales = 'free_x',
             nrow = 2)

# how did Iowa State do?
highlight_school <- "Iowa State"

mens_mw_clean |>
  ggplot(aes(x = time_decimal)) + 
  geom_histogram(fill = "gray80", alpha = 0.7) +
  geom_histogram(data = mens_mw_clean |> filter(school == highlight_school),
                 fill = "blue", alpha = 0.8) +
  geom_vline(data = mens_mw_clean |> 
               group_by(split) |> 
               summarize(avg_time = mean(time_decimal, na.rm = TRUE)),
             aes(xintercept = avg_time),
             color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_reverse() +
  facet_wrap(~split, scales = 'free_x', nrow = 2) +
  labs(title = paste("Times with average line, highlighting", highlight_school))

# largest jumps in places by split
mens_mw_clean |>
  group_by(split) |>
  filter(place_diff == max(place_diff,na.rm = T)) |>
  ungroup() |>
  arrange(split)

# visualizing places by split
max_place <- max(mens_mw_clean$place, na.rm = TRUE)

jumpers <- mens_mw_clean |>
  filter(split %in% c(2000,10000)) |>
  select(athlete,
         school,
         split,
         place) |>
  pivot_wider(names_from = split,
              values_from = place) |>
  clean_names() |>
  mutate(change = x2000-x10000) |>
  filter(dense_rank(desc(change)) <= 5)
#filter(school == 'Iowa')

library(ggrepel)
mens_mw_clean |>
  filter(athlete %in% jumpers$athlete) |>
  ggplot() +
  geom_line(mapping = aes(x = split,
                          y = place,
                          color = athlete),
            size = 1) +
  geom_point(mapping = aes(x = split,
                           y = place,
                           color = athlete),
             size = 2) +
  geom_text_repel(data = mens_mw_clean |> 
                    filter(athlete %in% jumpers$athlete, split == 10000),
                  aes(x = split, y = place, label = athlete, color = athlete),
                  hjust = 0, direction = "y", xlim = c(10000, NA), size = 3.5) +
  scale_y_reverse(breaks = c(1, seq(5, max_place, by = 5), max_place),
                  limits = c(max_place, 1)) +
  theme(legend.position = "none") +
  expand_limits(x = 11500)

# comparing first-half and second-half 5ks
mens_mw_clean |>
  filter(split %in% c(5000,10000)) |>
  select(school,
         athlete,
         class,
         split,
         time_decimal) |>
  pivot_wider(names_from = split,
              values_from = time_decimal) |>
  clean_names() |>
  # remove DNF
  filter(!is.na(x10000)) |>
  mutate(second_half_decimal = x10000-x5000,
         second_half_split_decimal = second_half_decimal-x5000,
         second_half_pct_change = round(((second_half_split_decimal/x5000)*-1)*100,1) ,
         first_half_time = decimal_to_time(x5000),
         second_half_time = decimal_to_time(second_half_decimal),
         split_time = decimal_to_time(abs(second_half_split_decimal))) |>
  arrange(second_half_split_decimal) |>
  as.data.frame()

finishing_kick <- mens_mw_clean |>
  group_by(athlete, school) |>
  arrange(athlete, split) |>
  mutate(
    segment_distance = split - lag(split, default = 0),
    segment_time = split_diff_decimal,
    pace_per_1000m = (segment_time / segment_distance) * 1000
  ) |>
  filter(!is.na(pace_per_1000m)) |>
  mutate(
    segment = case_when(
      split > 8000 ~ "last_2k",
      TRUE ~ "earlier"
    )
  ) |>
  group_by(athlete, school, segment) |>
  summarize(avg_pace = mean(pace_per_1000m, na.rm = TRUE), .groups = 'drop') |>
  pivot_wider(names_from = segment, values_from = avg_pace) |>
  mutate(
    kick_strength = abs(earlier - last_2k)  # Positive = strong finish
  ) |>
  arrange(desc(kick_strength))

finishing_kick |>
  #filter(kick_strength > 0) |>
  as.data.frame() |>
  mutate(earlier = decimal_to_time(earlier),
         last_2k = decimal_to_time(last_2k),
         kick_strength = decimal_to_time(kick_strength))