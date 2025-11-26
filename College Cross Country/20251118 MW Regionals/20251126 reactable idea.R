# possible gt or reactable?

source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251118 Mens MW Regional Data Cleaning.R')

library(reactable)
library(gt)
  
#   ----  runners who did not finish  ----

dnf_runners <- mens_mw_clean |>
  filter(split == 10000) |>
  filter(is.na(time_decimal)) |>
  pull(athlete)


#   basic stats ----

basic_stats <- mens_mw_clean |>
  filter(
    split == 10000
    ) |>
  select(
    athlete,
    school,
    class,
    final_place = place,
    final_time = time_formatted
  )

mens_mw_clean |>
  group_by(athlete) |>
  summarise(
    initial_place = first(place,na_rm = T),
    final_place = last(place,na_rm = T),
    final_time = last(time_formatted,na_rm = T),
    .groups = 'drop'
  ) |>
  mutate(
    place_change = initial_place-final_place
  )

# pace per km ----

split_pace_per_km <- mens_mw_clean |>
  group_by(
    athlete, 
    school,
    class
    ) |>
  mutate(
    cumulative_seconds = map_dbl(time_formatted, time_to_seconds),
    split_time = cumulative_seconds - lag(cumulative_seconds, default = 0),
    # Calculate distance between checkpoints
    split_length = split - lag(split, default = 0),
    # Calculate pace per km for each split
    pace_per_km = (split_time / split_length) * 1000
  ) |>
  ungroup() |>
  group_by(
    split
  ) |>
  mutate(
    median_pace_km = median(pace_per_km,na.rm = T),
    average_pace_km = mean(pace_per_km,na.rm = T)
  ) |>
  ungroup()

pacing_stats <- split_pace_per_km |>
  filter(!athlete %in% dnf_runners) |>
  group_by(
    athlete,
    class,
    school
  ) |>
  summarize(
    first_half_pace = round(median(pace_per_km[split <= 5000], na.rm = TRUE),3),
    second_half_pace = round(median(pace_per_km[split > 5000], na.rm = TRUE),3),
    pace_difference_5k = second_half_pace - first_half_pace,
    first_8k_half_pace = round(median(pace_per_km[split <= 8000], na.rm = TRUE),3),
    last_2k_half_pace = round(median(pace_per_km[split > 8000], na.rm = TRUE),3),
    pace_difference_8k = last_2k_half_pace - first_8k_half_pace,
    
    .groups = "drop"
  ) |>
  arrange((last_2k_half_pace)) |>
  as.data.frame()

# ----  bringing it together  ----
full_stats <- basic_stats |>
  inner_join(pacing_stats) |>
  arrange((final_place))

# ----  reactable elements  ----
group_5k <- c('first_half_pace','second_half_pace','pace_difference_5k')
group_last_kick <- c('first_8k_half_pace','last_2k_half_pace','pace_difference_8k')

# ----  the reactable ----

full_react <- reactable(
  full_stats,
  
  columnGroups = list(
    colGroup(name = "First vs Second 5K", columns = group_5k),
    colGroup(name = "First 8K vs Last 2K (Kick)", columns = group_last_kick)
  ),
  
  # column formatting
  columns = list(
    athlete = colDef(
      sticky = 'left',
      align = 'right',
      name = 'Athlete',
      minWidth = 150,
      style = list(fontWeight = "bold"),
      cell = function(value, index) {
        athlete_name <- value
        school_name <- full_stats$school[index]
        class_year <- full_stats$class[index]
        
        div(
          div(style = "font-weight: bold;", athlete_name),
          div(
            style = "font-size: 12px; color: #666; margin-top: 2px;",
            paste0(class_year, " - ", school_name)
          )
        )
      }
    ),
    school = colDef(show = FALSE),  # Hide the school column
    class = colDef(show = FALSE), 
    final_place = colDef(
      filterable = FALSE,
      name = 'Place',
      minWidth = 70,
      align = 'center'
      ),
    final_time = colDef(
      name = '10K Time',
      align = 'center',
      minWidth = 90,
    ),
    first_half_pace = colDef(
      filterable = FALSE,
      name = 'First 5K',
      minWidth = 90,
      align = "center",
      format = colFormat(digits = 1, suffix = " s/km")
      ),
    second_half_pace = colDef(
      filterable = FALSE,
      name = 'Last 5K',
      minWidth = 90,
      align = "center",
      format = colFormat(digits = 1, suffix = " s/km")
      ),
    pace_difference_5k = colDef(
      defaultSortOrder = 'asc',
      filterable = FALSE,
      #format = colFormat(digits = 3),
      align = 'center',
      name = 'Pace Diff',
      cell = function(value) {
        if (is.na(value)) return("")
        if (value < 0) {
          paste0("−", abs(round(value, 1)))
        } else {
          paste0("+", round(value, 1))
        }
      }
    ),
    first_8k_half_pace = colDef(
      filterable = FALSE,
      name = 'First 8K',
      minWidth = 90,
      align = "center",
      format = colFormat(digits = 1, suffix = " s/km")
      ),
    last_2k_half_pace = colDef(
      filterable = FALSE,
      name = 'Last 2K',
      minWidth = 90,
      align = "center",
      format = colFormat(digits = 1, suffix = " s/km")
      ),
    pace_difference_8k = colDef(
      filterable = FALSE,
      format = colFormat(digits = 3),
      name = 'Pace Diff',
      align = 'center',
      cell = function(value) {
        if (is.na(value)) return("")
        if (value < 0) {
          paste0("−", abs(round(value, 1)))
        } else {
          paste0("+", round(value, 1))
        }
      }
      )
    
  ),
  defaultSorted = c('pace_difference_5k'),
  pagination = FALSE,
  #filterable = TRUE,
  searchable = T,
  striped = T,
  compact = T,
  resizable = T,
  
  theme = reactableTheme(
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, 'Noto Sans', Arial, Consolas, sans-serif"),
    cellPadding = "8px 12px",
    searchInputStyle = list(width = "100%")
  )
  
  )

div(class = "standings",
    div(class = "title",
        h2("2019 Women's World Cup Predictions"),
        "Soccer Power Index (SPI) ratings and chances of advancing for every team"
    ),
    full_react,
    "Forecast from before 3rd group matches"
)