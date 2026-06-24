# Shiny App: NCAA Midwest Regional Athlete Comparison
# Load libraries and data

library(shiny)
library(ggplot2)
library(ggbeeswarm)
library(glue)
library(colorspace)
library(showtext)
library(dplyr)
library(tidyverse)
library(shadowtext)
#library(reactable)
library(ggtext)
library(ggpattern)

# Load your data sources
source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251118 Mens MW Regional Data Cleaning.R')
source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251129 school colors.R')

showtext_auto()
showtext_opts(dpi = 96)  # Use 96 for screen display

font_add_google("Fira Code", "ibm")
font_add_google("Noto Sans", "noto")

# Helper function to convert decimal to time
decimal_to_time <- function(x) {
  mins <- floor(x)
  secs <- round((x - mins) * 60, 1)
  sprintf("%d:%05.2f", mins, secs)
}

format_time_diff <- function(diff_seconds) {
  abs_diff <- abs(diff_seconds)
  if (abs_diff >= 60) {
    mins <- abs_diff %/% 60  # Use integer division instead of floor
    secs <- round(abs_diff %% 60)
    sprintf("%d:%02d", mins, secs)
  } else {
    paste0(round(abs_diff), " seconds")
  }
}

# Get unique athletes and schools

dnf_runners <- mens_mw_clean |>
  filter(split == 10000, is.na(time_decimal)) |>
  pull(athlete)

# Calculate pacing stats ONCE for all athletes
pacing_stats_global <- mens_mw_clean |>
  arrange(athlete, split) |>        # ensure correct order
  group_by(athlete, school, class) |>
  mutate(
    cumulative_seconds = time_decimal * 60,
    
    # Compute split-to-split segment time
    split_time = cumulative_seconds - lag(cumulative_seconds),
    
    # Compute segment distance
    split_length = split - lag(split),
    
    # Pace per km
    pace_per_km = (split_time / split_length) * 1000
  ) |>
  ungroup() |>
  group_by(split) |>
  mutate(
    median_pace_km  = median(pace_per_km, na.rm = TRUE),
    mean_pace_km    = mean(pace_per_km,  na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(!athlete %in% dnf_runners)

basic_stats_global <- mens_mw_clean |>
  filter(split == 10000) |>
  select(athlete, school, class, final_place = place, final_time = time_formatted)

athlete_list <- mens_mw_clean |> 
  filter(split == 10000, !is.na(time_decimal)) |>  # Only finishers at 10k
  distinct(athlete, school) |> 
  arrange(school, athlete)

# UI
ui <- fluidPage(
  
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  #titlePanel("NCAA Midwest Regional 10K - Athlete Comparison"),
  
  # Add the selector at the top level, before tabs
  fluidRow(
    column(
      width = 12,
      wellPanel(
        fluidRow(
          column(
            width = 6,
            selectInput(
              "athlete",
              "Select Athlete:",
              choices = setNames(sort(athlete_list$athlete), 
                                 sort(paste0(athlete_list$athlete, " (", athlete_list$school, ")"))),
              selected = sample(athlete_list$athlete, 1)
            )
          ),
          column(
            width = 6,
            br(),
            downloadButton("download_plot", "Download Comparison Plot")
          )
        )
      )
    )
  ),
  
  # Add tabs
  tabsetPanel(
    type = "tabs",
    
    # Tab 1: Athlete Comparison
    tabPanel(
      "Athlete Comparison",
      br(),
      plotOutput("density_plot", height = "200px", width = "1000px"),
      plotOutput("quasi_plot", height = "400px", width = "1000px")
    ),
    
    # Tab 2: Pacing vs. the field
    tabPanel(
      "Pacing per Split",
      br(),
      plotOutput("pacing_plot", height = "700px", width = "1000px")
    )
  )
)
# Server
server <- function(input, output, session) {
  
  # Reactive values based on selected athlete
  athlete_data <- reactive({
    req(input$athlete)
    
    athlete_var <- input$athlete
    
    school_var <- mens_mw_clean |>
      filter(athlete == athlete_var) |>
      pull(school) |>
      first()  # More explicit than distinct() |> pull()
    
    school_color_var <- school_colors$primary_color[school_colors$school == school_var][1]
    school_color_var_light <- lighten(school_color_var, amount = 0.5)
    
    # Filter to 10k split ONCE
    mens_10k_times <- mens_mw_clean |>
      filter(split == 10000) |>
      select(school, athlete, time_decimal, place) |>
      mutate(
        color_spotlight = factor(
          case_when(
            athlete == athlete_var ~ 'spotlight',
            school == school_var ~ 'teammates',  # Removed "| athlete == athlete_var" - redundant
            TRUE ~ 'other'
          ),
          levels = c('spotlight','teammates','other')
        )
      )
    
    athlete_time <- mens_10k_times$time_decimal[mens_10k_times$athlete == athlete_var]
    
    # Pre-calculate group medians
    group_medians <- mens_10k_times |>
      filter(!is.na(time_decimal)) |>  # Remove the spotlight filter
      mutate(
        # Recode spotlight as teammates for median calculation
        group_for_median = if_else(color_spotlight == "spotlight", "teammates", as.character(color_spotlight))
      ) |>
      group_by(group_for_median) |>
      summarise(
        median_time = median(time_decimal, na.rm = TRUE),
        y_position = max(density(time_decimal)$y),
        .groups = "drop"
      ) |>
      rename(color_spotlight = group_for_median) |>
      mutate(
        color_spotlight = factor(color_spotlight, levels = c('teammates', 'other')),
        label = if_else(color_spotlight == "teammates", school_var, "Others")
      )
    
    group_summary_splits <- pacing_stats_global |>
      filter(athlete == athlete_var | school == school_var) |>  # Filter FIRST before mutate
      mutate(
        color_spotlight = factor(
          if_else(athlete == athlete_var, 'spotlight', 'teammates'),
          levels = c('spotlight','teammates','other')
        )
      ) |>
      group_by(split, color_spotlight) |>
      summarise(
        median_pace_km = median(pace_per_km, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Separately calculate "other" if needed:
    group_summary_other <- pacing_stats_global |>
      filter(athlete != athlete_var, school != school_var) |>
      group_by(split) |>
      summarise(
        median_pace_km = median(pace_per_km, na.rm = TRUE),
        color_spotlight = factor('other', levels = c('spotlight','teammates','other')),
        .groups = 'drop'
      )
    
    group_summary_splits <- bind_rows(group_summary_splits, group_summary_other)
    
    # Pre-filter data subsets
    data_other <- mens_10k_times |> filter(color_spotlight == 'other')
    data_teammates <- mens_10k_times |> filter(color_spotlight == 'teammates')
    data_spotlight <- mens_10k_times |> filter(color_spotlight == 'spotlight')
    
    list(
      athlete_var = athlete_var,
      school_var = school_var,
      school_color_var = school_color_var,
      school_color_var_light = school_color_var_light,
      mens_10k_times = mens_10k_times,
      athlete_time = athlete_time,
      group_medians = group_medians,
      data_other = data_other,
      data_teammates = data_teammates,
      data_spotlight = data_spotlight,
      group_summary_splits = group_summary_splits
      # Remove basic_stats and pacing_stats - use global versions
    )
  })
  
  # Quasi-random plot
  output$quasi_plot <- renderPlot({
    data <- athlete_data()
    
    # Use pre-calculated values
    min_time <- floor(min(data$mens_10k_times$time_decimal, na.rm = TRUE))
    max_time <- ceiling(max(data$mens_10k_times$time_decimal, na.rm = TRUE))  # ceiling instead of round + 1
    
    ggplot(data$mens_10k_times, aes(x = 0, y = time_decimal)) + 
      
      # Use pre-filtered data
      geom_quasirandom(
        data = data$data_other,
        shape = 21,
        stroke = 1,
        width = 0.4,
        method = 'smiley',
        fill = 'gray80',
        color = 'white',
        size = 4,
        alpha = 0.6
      ) +
      
      geom_hline(
        data = data$group_medians |> filter(color_spotlight == 'other'),
        aes(yintercept = median_time),
        color = 'gray70',
        linetype = "solid",
        linewidth = 1.2
      ) +
      geom_hline(
        data = data$group_medians |> filter(color_spotlight == 'teammates'),
        aes(yintercept = median_time),
        color = 'white',
        linetype = "solid",
        linewidth = 1.8
      ) +
      geom_hline(
        data = data$group_medians |> filter(color_spotlight == 'teammates'),
        aes(yintercept = median_time),
        color = data$school_color_var_light,
        linetype = "solid",
        linewidth = 1.2
      ) +
      
      geom_quasirandom(
        data = data$data_teammates,
        shape = 21,
        stroke = 1,
        width = 0.4,
        method = 'smiley',
        fill = data$school_color_var,
        color = 'white',
        size = 6,
        alpha = 0.8
      ) +
      
      geom_hline(
        yintercept = data$athlete_time,
        color = 'white',
        linewidth = 1.8
      ) +
      geom_hline(
        yintercept = data$athlete_time,
        color = data$school_color_var,
        linewidth = 1.2
      ) +
      
      geom_quasirandom(
        data = data$data_spotlight,
        aes(fill = color_spotlight,
            color = color_spotlight == "spotlight"),
        shape = 21,
        stroke = 0.7,
        width = 0.4,
        method = 'smiley',
        size = 6
      ) +
      
      geom_shadowtext(
        data = data$data_spotlight,
        aes(label = athlete),  # No need for glue() for single variable
        position = position_quasirandom(width = 0.4, method = 'smiley'),
        size = 4,
        fontface = "bold",
        family = 'noto',
        hjust = 1.2,
        bg.color = 'black',
        color = data$school_color_var_light
      ) +
      coord_flip() +
      scale_y_reverse(
        limits = c(max_time, min_time),  # Avoid rev() function call
        breaks = seq(min_time, max_time, by = 1.5),
        labels = function(x) {
          mins <- floor(x)
          secs <- round((x - mins) * 60)
          sprintf("%d:%02d", mins, secs)
        }
      ) +
      scale_color_manual(
        values = c('TRUE' = 'black', 'FALSE' = 'white'),
        guide = 'none'
      ) +
      scale_alpha_manual(
        values = c("spotlight" = 1, "teammates" = 1, "other" = 0.6),
        guide = 'none'
      ) +
      scale_fill_manual(
        values = c(
          "spotlight" = data$school_color_var,
          "teammates" = data$school_color_var_light,
          "other" = "gray85"
        ),
        guide = 'none'
      ) +
      labs(
        x = '',
        y = '10K Race Time\n← Slower                                                     Faster →',
        caption = '\nVisualizations by Alex Elfering\nSource: NCAA & PrimeTime Timing'
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 13, color = 'gray80', family = 'ibm'),
        axis.title.x = element_text(size = 13, face = 'bold', family = 'noto'),
        plot.caption = element_text(size = 13, family = 'noto',hjust = 0, color = 'gray80'),
        axis.title.y = element_blank(),
        axis.line.y.left = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank()
      )
    
  }, res = 96)
  
  # Density plot - similar optimizations
  output$density_plot <- renderPlot({
    data <- athlete_data()
    
    min_time <- floor(min(data$mens_10k_times$time_decimal, na.rm = TRUE))
    max_time <- ceiling(max(data$mens_10k_times$time_decimal, na.rm = TRUE))
    
    # Get median times for title - ONLY CALCULATE ONCE
    teammates_median <- data$group_medians |> 
      filter(color_spotlight == "teammates") |> 
      pull(median_time)
    
    others_median <- data$group_medians |> 
      filter(color_spotlight == "other") |> 
      pull(median_time)
    
    # Format times
    athlete_time_formatted <- decimal_to_time(data$athlete_time)
    teammates_median_formatted <- decimal_to_time(teammates_median)
    others_median_formatted <- decimal_to_time(others_median)
    
    # Calculate differences (negative = faster)
    diff_vs_team <- data$athlete_time - teammates_median
    diff_vs_field <- data$athlete_time - others_median
    
    # Format differences as seconds
    team_diff_text <- case_when(
      abs(diff_vs_team * 60) <= 1 ~ "around the same time as",
      diff_vs_team < 0 ~ paste0(format_time_diff(diff_vs_team * 60), " faster than "),
      TRUE ~ paste0(format_time_diff(diff_vs_team * 60), " slower")
    )
    
    field_diff_text <- case_when(
      abs(diff_vs_field * 60) <= 1 ~ "around the same time as",
      diff_vs_field < 0 ~ paste0(format_time_diff(diff_vs_field * 60), " faster than "),
      TRUE ~ paste0(format_time_diff(diff_vs_field * 60), " slower")
    )
    
    ggplot(
      data$mens_10k_times, 
      aes(
        x = time_decimal, 
        color = color_spotlight, 
        fill = color_spotlight)
      ) +
      
      geom_density(
        data = data$data_other,
        alpha = 0.5, 
        linewidth = 1
      ) +
      geom_density(
        data = data$data_teammates,
        alpha = 0.5, 
        linewidth = 1
      ) +
      
      geom_errorbar(
        data = data$data_spotlight,
        mapping = aes(xmin = time_decimal - 0.2,
                      xmax = time_decimal + 0.2,
                      ymin= 0,
                      ymax = 0)
      ) +
      
      geom_point(
        data = data$data_spotlight,
        aes(y = 0),
        size = 4
      ) +
      
      scale_x_reverse(
        limits = c(max_time, min_time),
        breaks = seq(min_time, max_time, by = 1.5)
      ) +
      
      geom_shadowtext(
        data = data$group_medians,
        aes(x = median_time, y = y_position, label = label),
        size = 4,
        fontface = "bold",
        family = "noto"
      ) +
      scale_fill_manual(
        values = c(
          "spotlight" = data$school_color_var,
          "teammates" = data$school_color_var_light,
          "other" = "gray85"
        ),
        guide = 'none'
      ) +
      scale_color_manual(
        values = c(
          "spotlight" = data$school_color_var,
          "teammates" = data$school_color_var_light,
          "other" = "gray70"
        ),
        guide = 'none'
      ) +
      labs(
        title = glue::glue(
          "<b><span style='font-size:18pt'>NCAA Divion I Midwest Men's Regional 10K</span></b>"
        ),
        subtitle = glue::glue(
          "<b><span style='font-size:12pt; color:{data$school_color_var};'>{data$athlete_var}</span></b> finished {team_diff_text} ",
          "the <b><span style='font-size:12pt; color:{data$school_color_var};'>Median {data$school_var}</span></b> time at ",
          "<span style='font-family:ibm; font-weight:bold;'>{athlete_time_formatted}</span>. ",
          "<br>The median <b><span style='font-size:12pt; color:{data$school_color_var};'>{data$school_var}</span></b> runner finished with a time of ",
          "<span style='font-family:ibm; font-weight:bold;'>{teammates_median_formatted}</span>, while <b><span style='font-size:12pt; color:gray70;'>the rest of the field</span></b> finished with a median time of ",
          "<span style='font-family:ibm; font-weight:bold;'>{others_median_formatted}</span>."
        )
        #title = glue::glue(
        #  "<b><span style='font-size:16pt; color:{data$school_color_var};'>{data$athlete_var}</span> finished ",
        #  "<span style='font-size:14pt; color:{data$school_color_var};'>{team_diff_text}</span> than the median ",
        #  "<span style='font-size:14pt; color:{data$school_color_var};'>{data$school_var}</span> time</b>"
        #),
        #subtitle = glue::glue(
        #  "<span style='font-size:11pt;'>{data$athlete_var} finished with a time of {athlete_time_formatted} while {data$school_var} finished with a median time of {teammates_median_formatted}.<br>",
        #  "The rest of the field finished with a median time of {others_median_formatted}</span>"
        #)
        #subtitle = glue::glue(
        #  "<span style='font-size:11pt;'><b>{data$athlete_var}</b> finished with a time of ",
        #  "<span style='color:{data$school_color_var}; font-weight:bold;'>{athlete_time_formatted}</span> while <b>{data$school_var}</b> finished with a median time of ",
        #  "<span style='color:{data$school_color_var}; font-weight:bold;'>{teammates_median_formatted}</span>.<br>",
        #  "<b>The rest of the field</b> finished with a median time of ",
        #  "<span style='color:gray60; font-weight:bold;'>{others_median_formatted}</span></span>"
        #)
      ) +
      #theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = ggtext::element_markdown(
          size = 14,  # This sets the base size, but font-size in the HTML will override
          family = "noto", 
          #hjust = 0.5, 
          lineheight = 1.2,
          margin = margin(b = 5)  # Add some space below title
        ),
        plot.subtitle = ggtext::element_markdown(
          size = 11,
          family = "noto",
          #hjust = 0.5,
          lineheight = 1.3,
          color = "gray30"
        ),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank()
      )
    
  }, res = 96)
  
  output$pacing_plot <- renderPlot({
    data <- athlete_data()
    
    # Calculate the range data within the reactive context
    top_25_finishers <- pacing_stats_global |>
      filter(split == max(split)) |>
      filter(place <= 25) |>
      pull(athlete)
    
    top_25_range_pacing <- pacing_stats_global |>
      filter(athlete %in% top_25_finishers) |>
      filter(!is.na(pace_per_km)) |>  # Add this line
      group_by(split) |>
      summarise(
        slowest_pace = min(pace_per_km, na.rm = TRUE),
        fastest_pace = max(pace_per_km, na.rm = TRUE),
        .groups = 'drop'
      ) |>
      filter(!is.infinite(slowest_pace))|>
      filter(split != 2000) |>
      mutate(split = factor(split))
    
    top_25_range_lastk <- top_25_range_pacing |>
      filter(split == 10000) 
    
    range_pacing <- pacing_stats_global |>
      filter(!is.na(pace_per_km)) |>
      group_by(split) |>
      summarise(
        slowest_pace = min(pace_per_km, na.rm = TRUE),
        fastest_pace = max(pace_per_km, na.rm = TRUE),
        .groups = 'drop'
      ) |>
      filter(!is.infinite(slowest_pace)) |>
      filter(split != 2000) |>
      mutate(split = factor(split))
    
    slowest_pace_km <- min(range_pacing$slowest_pace)
    
    # Also convert in group_summary_splits
    plot_data <- data$group_summary_splits |>
      filter(split != 2000) |>
      mutate(split = factor(split))
    
    spotlight_athlete_data <- plot_data %>%
      filter(color_spotlight == 'spotlight') %>%
      filter(split == "4114")
    
    min_range <- floor(min(range_pacing$slowest_pace)) - 4
    max_range <- round(max(range_pacing$fastest_pace)) + 1
    
    # Create the plot
    ggplot(range_pacing) + 
      geom_ribbon(
        mapping = aes(x = split,
                      ymin = slowest_pace,
                      ymax = fastest_pace,
                      group = 1),
        fill = 'gray80',
        alpha = 0.4
        
      ) +
      geom_ribbon_pattern(
        data = top_25_range_pacing,
        aes(x = split, ymin = slowest_pace, ymax = fastest_pace,
            group = 1), 
        fill = "gray60",
        pattern_color = "white",
        pattern_fill = "white",
        pattern_angle = 135,
        pattern_density = 0.25,
        pattern_spacing = 0.01,
        pattern_key_scale_factor = 0.5,
        alpha = 0.4
      ) +
      geom_line(
        data = plot_data %>% filter(color_spotlight == 'other'),
        mapping = aes(x = split,
                      y = median_pace_km,
                      color = color_spotlight,
                      group = color_spotlight),  
        linewidth = 1.5
      ) +
      geom_line(
        data = plot_data %>% filter(color_spotlight == 'teammates'),
        mapping = aes(x = split,
                      y = median_pace_km,
                      group = color_spotlight),
        color = 'white',
        linewidth = 2.5
      ) +
      geom_line(
        data = plot_data %>% filter(color_spotlight == 'teammates'),
        mapping = aes(x = split,
                      y = median_pace_km,
                      color = color_spotlight,
                      group = color_spotlight),
        linetype = 'solid',
        linewidth = 1.5
      ) +
      geom_point(
        data = plot_data %>% filter(color_spotlight == 'spotlight'),
        mapping = aes(x = split,
                      y = median_pace_km),
        color = 'white',
        size = 2.5
      ) +
      geom_line(
        data = plot_data %>% filter(color_spotlight == 'spotlight'),
        mapping = aes(x = split,
                      y = median_pace_km,
                      group = color_spotlight),
        color = 'white',
        linewidth = 2.5
      ) +
      geom_line(
        data = plot_data %>% filter(color_spotlight == 'spotlight'),
        mapping = aes(x = split,
                      y = median_pace_km,
                      color = color_spotlight,
                      group = color_spotlight),
        linetype = 'solid',
        linewidth = 1.5
      ) +
      geom_point(
        data = plot_data %>% filter(color_spotlight == 'spotlight'),
        mapping = aes(x = split,
                      y = median_pace_km,
                      color = color_spotlight),
        size = 1.5
      ) +
      scale_color_manual(
        values = c(
          "spotlight" = data$school_color_var,
          "teammates" = data$school_color_var_light,
          "other" = "gray50"
        ),
        labels = c(
          #"spotlight" = data$athlete_var,
          "teammates" = glue("{data$school_var}"),
          "other" = "Rest of the Field"
        )
      ) +
      scale_y_continuous(
        limits = c(min_range, max_range),
        breaks = seq(min_range, max_range, by = 15),
        labels = function(x) {
          ifelse(x == max(x), paste0(x, " Seconds\nper km"), as.character(x))
        }
      ) +
      scale_x_discrete(
        labels = function(x) paste0(as.numeric(x)/1000, "K")
      ) +
      # annotation layer
      # ~~~explains the top 25 geom_ribbon()~~~
      annotate(
        "segment",
        x = 7.025, xend = 7.025,  # 8K is the 4th factor level
        y = top_25_range_lastk$slowest_pace, yend = top_25_range_lastk$fastest_pace,
        linetype = "dotted",
        color = "gray50",
        linewidth = 0.5
      ) +
      annotate(
        "text",
        x = 7.04,
        y = round((top_25_range_lastk$slowest_pace + top_25_range_lastk$fastest_pace)/2) ,
        label = "Top 25\nrunners\nstayed\nin this\nrange",
        #color = "gray40",
        size = 3,
        family = 'noto',
        hjust = 0,
        face = 'bold'
        #angle = 90,
        #vjust = -0.5
      ) +
      # ~~~ labels the slowest runner ~~~~
      annotate(
        "segment",
        x = 2.54, xend = 3,  # 8K is the 4th factor level
        y = 288, yend = 288,
        linetype = "dotted",
        color = "gray50",
        linewidth = 0.5
      ) +
      annotate(
        "text",
        x = 2.5,
        y = 288 ,
        label = "Slowest pace",
        #color = "gray40",
        size = 3,
        family = 'noto',
        hjust = 1,
        face = 'bold'
        #angle = 90,
        #vjust = -0.5
      ) +
      # ~~~ calling out where the race slowed down  ~~~
      annotate(
        "curve",
        x = 1.53, xend = 2,  # 8K is the 4th factor level
        y = 175, yend = 187,
        linetype = "dotted",
        color = "gray50",
        linewidth = 0.5,
        curvature = 0.3
      ) +
      annotate(
        "text",
        x = 1.5,
        y = 175 ,
        label = "Every runner slowed\nhere due to hills",
        #color = "gray40",
        size = 3,
        family = 'noto',
        hjust = 1,
        face = 'bold'
        #angle = 90,
        #vjust = -0.5
      ) +
      # ~~~ identify the spotlight athlete
      #geom_shadowtext(
      #  data = spotlight_athlete_data,
      #  mapping = aes(x = split,
      #                y = median_pace_km+3,
      #                label = data$athlete_var),  # Use the athlete name
      #  color = data$school_color_var_light,
      #  bg.color = 'black',
      #  size = 3.5,
      #  family = 'noto',
      ##  fontface = 'bold',
      #  hjust = -0.2,  # Position text to the right of the point
      #  vjust = 0.5
      #) +
      
      # LABELS AND THEMES
      labs(
        title = glue::glue(
          "<b><span style='font-size:18pt'>NCAA Divion I Midwest Men's Regional 10K</span></b>"
        ),
        subtitle = '',
           x = '\nSplit', 
           y = 'Slower ▲\n\n\n\n\n\n\n\n\nFaster ▼') +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.caption = element_text(size = 8, hjust = 0, color = 'gray80', family = 'noto'),
        axis.text.x = element_text(size = 10, color = 'gray50', family = 'ibm'),
        axis.text.y = element_text(size = 10, color = 'gray50', family = 'ibm'),
        axis.title.y.left = element_text(size = 10, family = 'noto',angle = 0,vjust = 0.5),
        axis.title.x = element_text(size = 11, family = 'noto'),
        axis.title.y = element_text(angle = 90, hjust = 1, vjust = 1, family = 'noto'),
        legend.title = element_blank(),
        legend.text = element_text(family = 'noto'),
        plot.title.position = "plot", 
        plot.caption.position = 'plot',
        axis.line.y.left = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = 'gray90', linetype = 'dashed')
      )
    
  }, res = 96)

  # Download handler
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", athlete_data()$athlete_var), "_comparison.png")
    },
    content = function(file) {
      data <- athlete_data()
      
      # Recreate the plot explicitly instead of using last_plot()
      p <- ggplot(data$mens_10k_times, aes(x = 0, y = time_decimal)) + 
        # ... (copy your entire quasi_plot code here)
        
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)