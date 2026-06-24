# day 2 of the 30DayChartChallenge?
# pictogram-ish
# how many times has ISU football finished 500 or better in the last 100 years?

library(ggplot2)
library(tidyverse)
library(waffle)
library(janitor)
library(stringr)

isu_fb_summary <- read.csv("~/GitHub/Sports/Iowa State FB Wins/ISU FB Summary Data.csv") |> clean_names()

isu_flags <- isu_fb_summary |>
  filter(year >= 1973) |>
  mutate(
    coach = str_trim(str_extract(coach_es,"^[^(]+")),
    bowl_clinched = bowl != '' ,
    winning_season = (w/(w+l+t))>0.5,
    winning_season_by_coach = 
      case_when(
        bowl != '' ~ coach,
        TRUE ~ 'No Bowl'
      )
  ) |>
  group_by(
    winning_season_by_coach
  ) |>
  summarise(
    count_season = n()
  )

isu_flags |>
  ggplot()+
  geom_waffle(mapping = aes(fill = winning_season_by_coach,values = count_season),
              color = 'white',
              n_rows = 5,
              flip = T) 