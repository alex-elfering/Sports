# the brand new shiny script

library(colorspace)
library(ggbeeswarm)
library(showtext)
library(ggtext)
#library(ggpattern)
library(shiny)
library(ggrepel)
library(gt)
library(data.table)
library(tidyverse)
#library(janitor)
#library(readxl)
library(stringr)
library(glue)
library(ggbump)

options(rsconnect.httr2 = FALSE)

# ----  data pulling  ----

init_xc_data <- read_rds("clean_xc_data.rds")
finishers <- read_rds("finishers.rds")
school_colors <- read_rds("school_colors.rds")

# ----  functions ----

assign_spotlight <- function(df, spotlight_athlete, spotlight_school){
  
  df |>
    mutate(color_spotlight = factor(
      case_when(athlete == spotlight_athlete ~ 'spotlight',
                school == spotlight_school ~ 'teammates',
                TRUE ~ "other"),
      levels = c('spotlight','teammates','other'))
    ) 
  
  
}
format_times <- function(time_decimal){
  sprintf('%d:%04.1f',floor(time_decimal),(time_decimal %% 1)*60)
}

# ----  init shiny variables  ----

showtext_auto()
showtext_opts(dpi = 96) 

font_add_google("Fira Code", "ibm")
font_add_google("Noto Sans", "noto")

ui <- fluidPage(
  
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  fluidRow(
    column(
      width = 12,
      wellPanel(
        fluidRow(
          column(
            width = 6,
            selectInput(
              "event",
              "Select Event:",
              choices = unique(sort(init_xc_data$event)),
              selected = '2025 Midwest Regional: M 10K'
            )
          ),
          column(                          
            width = 6,
            selectInput(
              "athlete",
              "Select Athlete:",
              choices = "Loading..."
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            align = "right",
            downloadButton("download_data", "Download Data")
          )
        )
      )
    )
  ),
  
  tabsetPanel(
    type = "tabs",
    
    # Tab 1: Athlete Comparison 
    #   examines how athlete's final time compares to the rest of the field
    tabPanel(
      "Athlete Comparison",
      plotOutput("density_plot", height = "600px", width = "1150px"),
      #gt_output("team_table") ,
      fluidRow(
        column(
          width = 5,
          gt_output("team_table")
        )
      )
    ),
    
    # Tab 2: Pacing comparison
    #   compares pacing against school or top 25 of race
    tabPanel(
      "Pacing per Split",
      br(),
      selectInput(
        "pace_comparison",
        "Compare against:",
        choices  = c("Team", "Top 25"),
        selected = "Team"
      ),
      plotOutput("pacing_plot", height = "600px", width = "800px"),
      #gt_output("pacing_table"),
      fluidRow(
        column(
          width = 5,
          gt_output("pacing_table")
        )
      )
    ),
    
    # tab #3: adjacent competitors
    #   compares race performance against adjacent competitors
    tabPanel(
      "Adjacent Competitors",
      br(),
      selectInput(
        "split_decision",
        "Split:",
        choices  = "Loading..." 
      ),
      fluidRow(
        column(
          width = 5,
          plotOutput("neighbor_places", height = "600px", width = "100%")
        ),
        column(
          width = 5,
          plotOutput("neighbor_pacing", height = "600px", width = "100%")
        )
      )
    ),
    tags$footer(
      HTML("Source: PrimeTime Timing; Black Squirrel Timing<br>Note: This only includes athletes who finished each race, respectively."),
      style = "font-family:noto; font-size:12pt;"
    )
  )
)
server <- function(input, output, session) {
  
  observeEvent(input$event, {
    athletes <- finishers |>
      filter(event == input$event) |>
      pull(athlete) |>
      sort()
    
    updateSelectInput(session,'athlete',choices = athletes)
  })
  
  observeEvent(input$event, {
    splits <- init_xc_data |>
      filter(event == input$event) |>
      mutate(distance_m = factor(distance_m)) |>
      distinct(distance_m) |>
      pull(distance_m) |>
      sort()
    
    updateSelectInput(session,'split_decision',choices = splits)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        str_replace_all(input$event, "[^A-Za-z0-9]+", "_"),
        "_",
        str_replace_all(input$athlete, "[^A-Za-z0-9]+", "_"),
        ".csv"
      )
    },
    content = function(file) {
      data <- athlete_data()$clean_xc_data
      fwrite(data, file)   # you already load data.table, so fwrite is fast/simple
    }
  )
  
  # reactives
  athlete_data <- reactive({
    req(input$athlete)
    
    test_athlete <- input$athlete
    test_event   <- input$event
    test_school  <- init_xc_data |>
      filter(event == input$event, athlete == input$athlete) |>
      pull(school) |>
      first()
    
    clean_xc_data <- init_xc_data |>
      filter(event == test_event) |>
      #filter(athlete %in% finishers$athlete) |>
      inner_join(finishers) |>
      group_by(event, athlete) |>
      ungroup() |>
      assign_spotlight(test_athlete, test_school)
    
    filter_final_splits <- clean_xc_data |>
      filter(distance_m == max(distance_m))
    
    group_summary_splits <- clean_xc_data |>
      group_by(distance_m, color_spotlight) |>
      summarise(median_pace_km = median(pace_km_min, na.rm = TRUE), .groups = "drop") |>
      mutate(distance_m = factor(distance_m))
    
    list(
      athlete_var          = test_athlete,
      event_var            = test_event,
      school_var           = test_school,
      final_splits         = filter_final_splits,
      group_summary_splits = group_summary_splits,
      clean_xc_data        = clean_xc_data
    )
  })
  spotlight_medians <- reactive({
    data <- athlete_data()
    filter_final_splits <- data$final_splits
    test_school <- data$school_var
    
    group_medians <- filter_final_splits |>
      filter(!is.na(time_decimal)) |>
      # re-classify selected "athlete" to create the overall median for their school, respectively
      mutate(group_for_median = if_else(color_spotlight == "spotlight", "teammates", as.character(color_spotlight))) |>
      group_by(group_for_median) |>
      summarise(median_time = median(time_decimal, na.rm = TRUE), .groups = "drop") |>
      transmute(
        color_spotlight = factor(group_for_median, levels = c('teammates', 'other')),
        median_time,
        label = if_else(group_for_median == "teammates", test_school, "Others")
      )
    
    list(
      group_medians    = group_medians,
      teammates_median = group_medians |> filter(color_spotlight == 'teammates') |> pull(median_time),
      others_median    = group_medians |> filter(color_spotlight == 'other') |> pull(median_time)
    )
  })
  school_colors_data <- reactive({
    test_school <- athlete_data()$school_var   # don't re-derive, reuse
    
    school_color_var       <- school_colors$primary_color[school_colors$school == test_school][1]
    school_color_var_light <- lighten(school_color_var, 0.5)
    
    list(
      school_color_var       = '#f46d43',
      school_color_var_light = '#fdae61'
    )
  })
  athlete_pacing <- reactive({
    req(input$pace_comparison)
    
    data <- athlete_data()
    sc   <- school_colors_data()
    
    test_athlete            <- data$athlete_var
    test_school             <- data$school_var
    clean_xc_data           <- data$clean_xc_data
    filter_final_splits     <- data$final_splits
    school_color_var_light  <- sc$school_color_var_light
    options_select          <- input$pace_comparison
    
    if (options_select == 'Top 25') {
      top_n_finishers <- filter_final_splits |>
        filter(place <= 25) |>
        pull(athlete)
      
      pacing_split <- clean_xc_data |>
        mutate(distance_m = factor(distance_m)) |>
        filter(athlete %in% top_n_finishers) |>
        group_by(distance_m) |>
        summarise(
          slowest_pace = min(pace_km_min, na.rm = TRUE),
          fastest_pace = max(pace_km_min, na.rm = TRUE),
          median_pace  = median(pace_km_min, na.rm = TRUE),
          .groups = 'drop'
        )
      
      median_color <- 'gray60'
      
    } else if (options_select == 'Team') {
      pacing_split <- clean_xc_data |>
        mutate(distance_m = factor(distance_m)) |>
        filter(school == test_school) |>
        group_by(distance_m) |>
        summarise(
          slowest_pace = min(pace_km_min, na.rm = TRUE),
          fastest_pace = max(pace_km_min, na.rm = TRUE),
          median_pace  = median(pace_km_min, na.rm = TRUE),
          .groups = 'drop'
        )
      
      median_color <- school_color_var_light
    }
    
    options_select <- ifelse(options_select == 'Team', test_school, options_select)
    
    pacing_by_split <- clean_xc_data |>
      mutate(distance_m = factor(distance_m)) |>
      group_by(distance_m) |>
      summarise(
        slowest_pace = min(pace_km_min, na.rm = TRUE),
        fastest_pace = max(pace_km_min, na.rm = TRUE),
        .groups = 'drop'
      )
    
    list(
      athlete_var     = test_athlete,
      options_select  = options_select,
      pacing_split    = pacing_split,
      pacing_by_split = pacing_by_split,
      median_color    = median_color
    )
  })
  adjacent_competitors <- reactive({
    
    data <- athlete_data()
    
    test_athlete <- data$athlete_var
    clean_xc_data    <- data$clean_xc_data 
    
    up_until_distance <- input$split_decision
    neighbors <- 1
    
    split_data <- clean_xc_data |> filter(distance_m == up_until_distance)
    
    athlete_place <- split_data |>
      filter(athlete == test_athlete) |>
      pull(place)
    
    max_place <- max(split_data$place, na.rm = TRUE)
    min_place <- min(split_data$place, na.rm = TRUE)
    
    #window_size <- neighbors * 2
    
    range_start <- athlete_place - neighbors
    range_end <- athlete_place + neighbors
    
    if (range_start < min_place) {
      range_end <- range_end + (min_place - range_start)
      range_start <- min_place
    } else if (range_end > max_place) {
      range_start <- range_start - (range_end - max_place)
      range_end <- max_place
    }
    
    neighbor_range <- range_start:range_end
    
    athletes_pulled <- clean_xc_data |>
      filter(distance_m == up_until_distance) |>
      filter(place %in% neighbor_range) |>
      arrange(place) |>
      pull(athlete)
    
    # exclude the spotlighter before assigning Athlete_N labels
    neighbor_athletes <- setdiff(athletes_pulled, test_athlete)
    
    neighbor_labels <- paste0("Athlete_", seq_along(neighbor_athletes))
    label_map <- setNames(neighbor_labels, neighbor_athletes)
    
    athlete_data_filtered <- clean_xc_data |>
      filter(athlete %in% athletes_pulled) |>
      arrange(athlete, distance_m) |>
      mutate(
        labeler_label = if_else(athlete == test_athlete, "Spotlighter", label_map[athlete]),
        labeler_label = factor(labeler_label, levels = c("Spotlighter", neighbor_labels)),
        athlete = forcats::fct_relevel(factor(athlete), test_athlete, after = Inf)
      )
    
    list(
      athlete_var     = test_athlete,
      athlete_data_filtered = athlete_data_filtered,
      clean_xc_data= clean_xc_data,
      up_until_distance = up_until_distance,
      neighbor_athletes = neighbor_athletes
    )
    
  })
  
  # overview
  output$density_plot <- renderPlot({
    data <- athlete_data()
    sch_colors <- school_colors_data()
    grp_medians <- spotlight_medians()
    
    base_style <- "font-family:noto; font-size:12pt; font-weight:bold;"
    
    test_athlete           <- data$athlete_var
    test_event             <- data$event_var
    test_school            <- data$school_var
    school_color_var       <- sch_colors$school_color_var
    school_color_var_light <- sch_colors$school_color_var_light
    filter_final_splits    <- data$final_splits 
    clean_xc_data    <- data$clean_xc_data
    group_medians <- grp_medians$group_medians
    teammates_median <- grp_medians$teammates_median
    others_median <- grp_medians$others_median
    
    athlete_place <- clean_xc_data |>
      filter(distance_m == max(distance_m)) |>
      filter(athlete == test_athlete,
             distance_m == max(distance_m)) |>
      pull(place)
    
    
    final_athlete_spotlight_times <- filter_final_splits |>
      filter(color_spotlight == 'spotlight') |>
      select(time_decimal,
             time_new)
    
    athlete_time_decimal <- final_athlete_spotlight_times$time_decimal
    athlete_time_formatted <- final_athlete_spotlight_times$time_new
    
    teammate_median_formatted <- sprintf("%d:%04.1f", floor(teammates_median), (teammates_median %% 1) * 60)
    others_median_formatted <- sprintf("%d:%04.1f", floor(others_median), (others_median %% 1) * 60)
    
    min_time <- floor(min(filter_final_splits$time_decimal, na.rm = TRUE))
    max_time <- ceiling(max(filter_final_splits$time_decimal, na.rm = TRUE))
    
    diff_vs_team <- athlete_time_decimal - teammates_median
    diff_abs <- abs(diff_vs_team)
    diff_secs <- diff_abs * 60
    
    team_diff_text <- case_when(
      diff_secs <= 1 ~ "around the same time as",
      
      diff_vs_team < 0 & diff_secs < 60 ~ sprintf("%.1f seconds faster than", diff_secs),
      diff_vs_team > 0 & diff_secs < 60 ~ sprintf("%.1f seconds slower than", diff_secs),
      
      diff_vs_team < 0 ~ paste0(sprintf("%d:%04.1f", floor(diff_abs), (diff_abs %% 1) * 60), " faster than "),
      TRUE ~ paste0(sprintf("%d:%04.1f", floor(diff_abs), (diff_abs %% 1) * 60), " slower than ")
    )
    
    ggplot(filter_final_splits, 
           aes(x = 0, 
               y = time_decimal,
               fill = color_spotlight,
               color = color_spotlight,
               alpha = color_spotlight)
    ) + 
      geom_hline(data = filter(group_medians, 
                               color_spotlight == 'other'),
                 aes(yintercept = median_time), 
                 color = 'gray70', 
                 linewidth = 1.5) +
      geom_hline(data = filter(group_medians, 
                               color_spotlight == 'teammates'),
                 aes(yintercept = median_time), 
                 color = school_color_var, 
                 linewidth = 1.5) +
      # beeswarm
      geom_quasirandom(data = filter_final_splits, 
                       shape = 21, 
                       stroke = 1, 
                       width = 0.4, 
                       method = 'tukeyDense',
                       #color = 'white', 
                       size = 4) +
      
      geom_label_repel(
        data = filter(filter_final_splits, color_spotlight == 'spotlight'),
        aes(label = test_athlete),  # Can also be a static string like "Selected"
        nudge_x = 0.5,              # Pushes the label vertically (due to coord_flip) above the swarm
        min.segment.length = 0,     # Forces the arrow to always draw
        segment.color = "black",
        segment.size = 0.6,
        arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
        family = "noto",            # Matches the fonts in your theme
        fontface = "bold",
        fill = "white",
        color = "black",
        show.legend = FALSE
      ) +
      
      
      scale_color_manual(
        values = c('other' = 'white',
                   'teammates' = school_color_var,
                   'spotlight' = 'black')
      ) +
      coord_flip() +
      scale_y_reverse(limits = c(max_time, min_time), 
                      breaks = seq(min_time, max_time, by = 1),
                      labels = function(x) {
                        total_seconds <- round(x * 60)
                        mins <- total_seconds %/% 60
                        secs <- total_seconds %% 60
                        sprintf('%d:%02d', mins, secs)
                      }) +
      scale_alpha_manual(
        values = c("spotlight" = 1, "teammates" = 1, "other" = 0.5), guide = 'none'
      ) +
      scale_fill_manual(
        values = c("spotlight" = school_color_var, "teammates" = school_color_var_light, "other" = "gray75"), 
        guide = 'none'
      ) +
      labs(y = NULL, 
           y = NULL,
           title = glue("<b><span style='font-size:18pt'>{test_event}</span></b>"),
           subtitle = glue(
             "<b><span style='font-size:12pt; color:{school_color_var};'>{test_athlete}</span></b> finished #{athlete_place} at ",
             "<span style='font-family:ibm; font-weight:bold;'>{athlete_time_formatted}</span>, ",
             "{team_diff_text} the median <b><span style='font-size:12pt; color:{school_color_var};'>{test_school}</span></b> runner. ",
             "<br>The median <b><span style='font-size:12pt; color:{school_color_var};'>{test_school}</span></b> runner finished with a time of ",
             "<span style='font-family:ibm; font-weight:bold;'>{teammate_median_formatted}</span>, while <b><span style='font-size:12pt; color:gray70;'>the rest of the field</span></b> finished with a median time of ",
             "<span style='font-family:ibm; font-weight:bold;'>{others_median_formatted}</span>.")
      ) +
      annotate("text", x = -0.6, y = min_time + 0.5, label = "Faster --->", 
               family = "ibm", size = 3.5, color = "gray20") +
      annotate("text", x = -0.6, y = max_time - 0.5, label = "<--- Slower", 
               family = "ibm", size = 3.5, color = "gray20") +
      theme_minimal() +
      theme(
        legend.position = "none", 
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 13, color = 'gray80', family = 'ibm'),
        axis.title.x = element_text(size = 13, face = 'bold', family = 'noto'),
        plot.caption = element_text(size = 13, family = 'noto', hjust = 0, color = 'gray80'),
        plot.subtitle = ggtext::element_markdown(family = "Noto Sans", size = 12,lineheight = 1.2),
        plot.title = ggtext::element_markdown(lineheight = 1.2),
        axis.title.y = element_blank(), 
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_blank(), 
        panel.grid = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
      )
    
    
  }, res = 96)
  output$team_table <- render_gt({
    data <- athlete_data()
    sch_colors <- school_colors_data()
    grp_medians <- spotlight_medians()
    
    base_style <- "font-family:noto; font-size:12pt; font-weight:bold;"
    
    test_athlete           <- data$athlete_var
    test_event             <- data$event_var
    test_school            <- data$school_var
    school_color_var       <- sch_colors$school_color_var
    school_color_var_light <- sch_colors$school_color_var_light
    filter_final_splits    <- data$final_splits 
    clean_xc_data    <- data$clean_xc_data
    group_medians <- grp_medians$group_medians
    teammates_median <- grp_medians$teammates_median
    others_median <- grp_medians$others_median
    
    athlete_place <- clean_xc_data |>
      filter(distance_m == max(distance_m)) |>
      filter(athlete == test_athlete,
             distance_m == max(distance_m)) |>
      pull(place)
    
    
    final_athlete_spotlight_times <- filter_final_splits |>
      filter(color_spotlight == 'spotlight') |>
      select(time_decimal,
             time_new)
    
    teammates_median <- group_medians |>
      filter(color_spotlight == 'teammates') |>
      pull(median_time)
    
    others_median <- group_medians |>
      filter(color_spotlight == 'other') |>
      pull(median_time)
    
    distribution_table <- filter_final_splits |>
      #mutate(percentile = round(percent_rank(desc(time_decimal)),2) ) |>
      #arrange(desc(time_decimal)) |>
      #mutate(size_of_lead = (lead(time_decimal)-time_decimal)*60,
      #       size_of_lag = (lag(time_decimal)-time_decimal)*60 ) |>
      filter(school == test_school) |>
      select(athlete,
             place,
             time_decimal) |>
      mutate(school_diff = round((time_decimal-teammates_median)*60,1),
             field_diff = round((time_decimal-others_median)*60,1 )) |>
      arrange(time_decimal) |>
      mutate(
        time_decimal = format_times(time_decimal)
      ) 
    
    distribution_table |>
      gt() |>
      cols_label(
        athlete      = "Athlete",
        place        = "Place",
        time_decimal = "Time",
        school_diff = glue('{test_school}'),
        field_diff = glue('Field')
      ) |>
      tab_style(
        style     = cell_fill(color = school_color_var_light),
        locations = cells_body(rows = athlete == test_athlete)
      ) |>
      fmt(
        columns = c(school_diff, field_diff),
        fns = function(x) {
          dplyr::case_when(
            x >  0 ~ paste0("+", x),
            x == 0 ~ "At Median",
            x <  0 ~ as.character(x)
          )
        }
      ) |>
      tab_style(
        style     = cell_text(style = "italic"),
        locations = cells_body(
          columns = c(school_diff, field_diff),  # both columns
          rows    = school_diff == 0
        )
      ) |>
      tab_style(                                  # was tabs_style()
        style     = cell_text(                    # was style = font = ...
          font = google_font("Fira Code")
        ),
        locations = cells_body(                  # columns goes inside here
          columns = c(school_diff, field_diff,time_decimal)
        )
      ) |>
      tab_style(
        style = cell_text(
          font   = google_font("Noto Sans"),
          weight = "bold",
          color  = "black",
          size   = 12
        ),
        locations = cells_column_labels(columns = everything())
      ) |>
      tab_spanner(
        label   = "vs. Median (secs)",
        columns = c(school_diff, field_diff)
      ) |>
      tab_header(
        title    = glue("{test_athlete} - Team Standings"),
        subtitle = glue("Seconds relative to {test_school} and field medians")
      ) |>
      tab_style(
        style = cell_text(
          font   = google_font("Noto Sans"),
          size   = 14,
          weight = "bold",
          color  = school_color_var,
          align = 'left'
        ),
        locations = cells_title(groups = "title")
      ) |>
      tab_style(
        style = cell_text(
          font  = google_font("Noto Sans"),
          size  = 12,
          align = 'left'
        ),
        locations = cells_title(groups = "subtitle")
      ) |>
      tab_options(
        table.border.top.style    = "none",  
        table.border.bottom.style = "none"   
      )
  })
  output$team_spread_table <- render_gt({
    data <- athlete_data()
    sch_colors <- school_colors_data()
    grp_medians <- spotlight_medians()
    
    base_style <- "font-family:noto; font-size:12pt; font-weight:bold;"
    
    test_athlete           <- data$athlete_var
    test_event             <- data$event_var
    test_school            <- data$school_var
    school_color_var       <- sch_colors$school_color_var
    school_color_var_light <- sch_colors$school_color_var_light
    filter_final_splits    <- data$final_splits 
    clean_xc_data    <- data$clean_xc_data
    group_medians <- grp_medians$group_medians
    teammates_median <- grp_medians$teammates_median
    others_median <- grp_medians$others_median
    
    school_top5_spreads <- filter_final_splits |>
      #filter(school == test_school) |>
      arrange(time_decimal) |>
      mutate(final_pace_per_km = time_decimal/(distance_m/1000)) |>
      group_by(school) |>
      filter(row_number() <= 5) |>
      summarise(fastest_time = min(time_decimal),
                slowest_time = max(time_decimal),
                median_pace_km = median(final_pace_per_km),
                runners = n(),
                .groups = 'drop') |>
      mutate(spread = (slowest_time-fastest_time)*60 ) |>
      arrange(median_pace_km) |>
      filter(runners == 5) |>
      select(-runners) |>
      mutate(fastest_time = format_times(fastest_time),
             median_pace_km = format_times(median_pace_km),
             slowest_time = format_times(slowest_time))
    
    school_top5_spreads |>
      gt() |>
      tab_header(
        title    = glue("{test_school} - Team Standings"),
        subtitle = glue("Seconds relative to {test_school} and field medians")
      ) |>
      tab_style(
        style = cell_text(
          font   = google_font("Noto Sans"),
          size   = 14,
          weight = "bold",
          color  = school_color_var,
          align = 'left'
        ),
        locations = cells_title(groups = "title")
      ) |>
      tab_style(
        style = cell_text(
          font  = google_font("Noto Sans"),
          size  = 12,
          align = 'left'
        ),
        locations = cells_title(groups = "subtitle")
      ) |>
      tab_options(
        table.border.top.style    = "none",  
        table.border.bottom.style = "none"   
      )
    })
  
  # pacing
  output$pacing_table <- render_gt({
    data         <- athlete_data()
    pacing_data  <- athlete_pacing()   # reuse instead of recompute
    req(input$pace_comparison)
    
    test_athlete            <- pacing_data$athlete_var
    test_school             <- data$school_var
    school_color_var        <- school_colors_data()$school_color_var
    group_summary_splits    <- data$group_summary_splits
    
    pacing_split    <- pacing_data$pacing_split
    pacing_by_split <- pacing_data$pacing_by_split
    options_select  <- pacing_data$options_select
    
    pacing_range <- pacing_split |>
      mutate(
        slowest_pace = format_times(slowest_pace),
        fastest_pace = format_times(fastest_pace)
      )
    
    spotlight_times <- group_summary_splits |>
      filter(color_spotlight == 'spotlight') |>
      select(distance_m, spotlight_times = median_pace_km)
    
    compare_pacing <- pacing_range |>
      full_join(spotlight_times, by = "distance_m") |>
      mutate(distance = (spotlight_times - median_pace) * 60) |>
      unite(range_of_pace, c(slowest_pace, fastest_pace), sep = '-', na.rm = TRUE) |>
      mutate(
        spotlight_times = format_times(spotlight_times),
        median_pace     = format_times(median_pace),
        distance_k      = as.integer(as.character(distance_m)) / 1000
      ) |>
      select(distance_k, range_of_pace, median_pace, spotlight_times, distance)
    
    compare_pacing |>
      gt() |>
      fmt(
        columns = c(distance),
        fns = function(x) {
          dplyr::case_when(
            x >  0 ~ paste0("+", round(x,2)),
            x == 0 ~ "At Median",
            x <  0 ~ as.character(round(x,2)),
            TRUE ~ '-'
          )
        }
      ) |>
      cols_label(
        distance_k = "Split (km)",
        range_of_pace = "Pace Range",
        median_pace = "Median",
        spotlight_times = glue('{test_athlete}'),
        distance = glue("vs {options_select} median")
      ) |>
      tab_spanner(
        label   = glue("{options_select}"),
        columns = c(range_of_pace, median_pace)
      ) |>
      tab_style(
        style = cell_text(
          font   = google_font("Noto Sans"),
          weight = "bold",
          color  = "black",
          size   = 12
        ),
        locations = cells_body(columns = c(spotlight_times,distance) )
      ) |>
      tab_style(
        style = cell_text(
          font   = google_font("Noto Sans"),
          color  = "black",
          size   = 12
        ),
        locations = cells_body(columns = everything() )
      ) |>
      tab_header(
        title    = glue("{test_athlete} vs {options_select} Pace Range"),
        subtitle = glue("Seconds relative to median {options_select} pace per km by split")
      ) |>
      tab_style(
        style = cell_text(
          font   = google_font("Noto Sans"),
          size   = 14,
          weight = "bold",
          color  = school_color_var,
          align = 'left'
        ),
        locations = cells_title(groups = "title")
      ) |>
      tab_style(
        style = cell_text(
          font  = google_font("Noto Sans"),
          size  = 12,
          align = 'left'
        ),
        locations = cells_title(groups = "subtitle")
      ) |>
      tab_options(
        table.border.top.style    = "none",  
        table.border.bottom.style = "none"   
      ) |>
      tab_style(
        style = cell_fill(color = "gray90"),
        locations = cells_body(columns = c(range_of_pace,median_pace))
      )
    
  })
  output$pacing_plot <- renderPlot({
    
    data        <- athlete_data()
    sc          <- school_colors_data()
    pacing_data <- athlete_pacing()
    req(input$pace_comparison) 
    
    test_athlete            <- pacing_data$athlete_var
    test_school             <- data$school_var
    school_color_var        <- sc$school_color_var
    school_color_var_light  <- sc$school_color_var_light
    clean_xc_data           <- data$clean_xc_data 
    group_summary_splits    <- data$group_summary_splits
    filter_final_splits     <- data$final_splits
    pacing_split            <- pacing_data$pacing_split
    pacing_by_split         <- pacing_data$pacing_by_split
    median_color            <- pacing_data$median_color
    options_select          <- pacing_data$options_select
    
    spotlight_final_time <- group_summary_splits |>
      filter(as.numeric(distance_m) == max(as.numeric(distance_m)),
             color_spotlight == 'spotlight') |>
      pull(median_pace_km)
    
    selection_median_time <- pacing_split |>
      filter(as.numeric(distance_m) == max(as.numeric(distance_m))) |>
      pull(median_pace)
    
    init_pacing_range <- pacing_split |>
      filter(as.numeric(distance_m) == min(as.numeric(distance_m)))
    
    beg_range <- init_pacing_range$slowest_pace
    end_range <- init_pacing_range$fastest_pace
    
    spotlight_selection_diff <- round((spotlight_final_time - selection_median_time) * 60, 1)
    
    #beg_segment <- ifelse(spotlight_selection_diff < 0, selection_median_time, spotlight_final_time)
    #end_segment <- ifelse(spotlight_selection_diff < 0, spotlight_final_time, selection_median_time)
    
    diff_label <- case_when(
      spotlight_selection_diff < 0 ~ paste0(spotlight_selection_diff, glue(' secs/km')),
      TRUE ~ paste0("+", spotlight_selection_diff, glue(' secs/km'))
    )
    
    min_pacing <- min(c(pacing_split$fastest_pace, pacing_by_split$slowest_pace))
    max_pacing <- max(c(pacing_by_split$fastest_pace, pacing_by_split$slowest_pace))
    
    n_splits <- length(unique(group_summary_splits$distance_m))
    last_split_position <- n_splits
    
    ggplot(pacing_by_split) + 
      geom_ribbon(data = pacing_split,
                  aes(x = distance_m, ymin = slowest_pace, ymax = fastest_pace, group = 1), 
                  fill = "gray60", alpha = 0.4) +
      geom_point(pacing_split, mapping = aes(x = distance_m, y = median_pace, group = 1),
                 color = 'white', size = 1.8) +
      geom_line(pacing_split, mapping = aes(x = distance_m, y = median_pace, group = 1),
                linewidth = 1.5, color = 'white') +
      geom_point(pacing_split, mapping = aes(x = distance_m, y = median_pace, group = 1),
                 color = median_color, size = 1.4) +
      geom_line(pacing_split, mapping = aes(x = distance_m, y = median_pace, group = 1),
                linewidth = 1.1, color = median_color) +
      geom_point(data = group_summary_splits |> filter(color_spotlight == 'spotlight'),
                 mapping = aes(x = distance_m, y = median_pace_km, group = color_spotlight),
                 color = 'white', size = 1.8) +
      geom_line(data = group_summary_splits |> filter(color_spotlight == 'spotlight'),
                mapping = aes(x = distance_m, y = median_pace_km, group = color_spotlight),
                color = 'white', linewidth = 1.5) +
      geom_point(data = group_summary_splits |> filter(color_spotlight == 'spotlight'),
                 mapping = aes(x = distance_m, y = median_pace_km, group = color_spotlight),
                 color = school_color_var, size = 1.4) +
      geom_line(data = group_summary_splits |> filter(color_spotlight == 'spotlight'),
                mapping = aes(x = distance_m, y = median_pace_km, group = color_spotlight),
                color = school_color_var, linewidth = 1.1) +
      scale_x_discrete(labels = function(x) paste0(as.numeric(x)/1000, "K")) +
      scale_y_reverse(
        expand = expansion(mult = c(0.05, 0.05)),
        labels = function(x) {
          ifelse(is.na(x), NA_character_, {
            total_seconds <- round(x * 60)
            mins <- total_seconds %/% 60
            secs <- total_seconds %% 60
            formatted <- sprintf('%d:%02d', mins, secs)
            ifelse(x == min(x, na.rm = TRUE), paste0(formatted, '\nper km'), formatted)
          })
        },
        breaks = seq(min_pacing, max_pacing, by = 0.15)
      ) +
      annotate("segment", x = 0.95, xend = 0.95, y = beg_range, yend = end_range,
               linetype = "solid", color = "black") +
      annotate("text", x = 0.9, y = (beg_range + end_range) / 2,
               label = glue('{options_select}\nrange'), size = 3, family = 'noto', hjust = 1) +
      annotate("segment", x = last_split_position + 0.055, xend = last_split_position + 0.055,
               y = selection_median_time, yend = spotlight_final_time,
               linetype = "solid", color = "black",
               arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
      annotate("text", x = last_split_position + 0.06,
               y = (selection_median_time + spotlight_final_time) / 2,
               label = diff_label, size = 3, family = 'noto', hjust = 0) +
      labs(title = glue("<b><span style='font-family:noto;font-size:18pt'>Where did <span style ='color:{school_color_var}'>{test_athlete}</span> speed up or slow down?</span></b>"),
           subtitle = glue("<span style='font-family:noto;font-size:12pt'>Comparing Pace per Km against the median <b>{options_select} </b>pace by split<br>"),
           x = '\nSplit', y = NULL) +
      coord_cartesian(clip = "off") +
      theme_minimal() +
      theme(legend.position = "top",
            axis.text.x = element_text(size = 10, color = 'gray50', family = 'ibm'),
            axis.text.y = element_text(size = 10, color = 'gray50', family = 'ibm'),
            axis.title.y.left = element_markdown(size = 10, family = 'noto', angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 11, family = 'noto'),
            legend.title = element_blank(),
            legend.text = element_text(family = 'noto'),
            plot.subtitle = ggtext::element_textbox_simple(lineheight = 1.2, margin = margin(t = 5, b = 15)),
            plot.title = ggtext::element_textbox_simple(lineheight = 1.2, margin = margin(b = 5)),
            plot.title.position = "plot",
            axis.line = element_blank(), axis.ticks = element_blank(),
            panel.background = element_blank(), panel.grid.minor = element_blank(),
            plot.margin = margin(t = 10, r = 60, b = 10, l = 40),
            panel.grid.major = element_blank(),
            panel.grid.major.y = element_line(color = 'gray90', linetype = 'dashed'))
    
  }, res = 96)
  
  # adjacent competitors
  output$neighbor_places <- renderPlot({
    
    data <- adjacent_competitors()
    
    test_athlete <- data$athlete_var
    clean_xc_data    <- data$clean_xc_data 
    athlete_data_filtered <- data$athlete_data_filtered
    up_until_distance <- as.integer(data$up_until_distance)
    neighbor_athletes <- data$neighbor_athletes
    
    neighbor_labels <- levels(athlete_data_filtered$labeler_label)[-1]  # everything except "Spotlighter"
    palette_colors <- c('#e08214', '#b2abd2', '#8073ac')[seq_len(length(neighbor_labels) + 1)]
    color_values <- setNames(palette_colors, c("Spotlighter", neighbor_labels))
    legend_labels <- setNames(c(test_athlete, neighbor_athletes), c("Spotlighter", neighbor_labels))
    
    dist_label <- up_until_distance/1000
    
    athlete_data_filtered |>
      ggplot() + 
      geom_vline(xintercept = factor(up_until_distance),
                 alpha = 0.6,
                 linewidth = 0.5) +
      geom_bump(mapping = aes(x = factor(distance_m),
                              y = place,
                              color = labeler_label,
                              group = athlete),
                linewidth = 1,
                #linetype = 'dashed',
                alpha = 0.5) +
      geom_bump(athlete_data_filtered |> filter(distance_m <= up_until_distance),
                mapping = aes(x = factor(distance_m),
                              y = place,
                              color = labeler_label,
                              group = athlete),
                linewidth = 1) +
      geom_point(athlete_data_filtered |> filter(distance_m <= up_until_distance),
                 mapping = aes(x = factor(distance_m),
                               y = place,
                               color = labeler_label,
                               group = athlete),
                 size = 1.5) +
      geom_text_repel(
        data = athlete_data_filtered |> filter(distance_m == up_until_distance),
        mapping = aes(x = factor(distance_m),
                      y = place,
                      label = stringr::str_wrap(paste0('#',place)),
                      color = labeler_label),
        fontface = 'bold',
        #nudge_x = 0.5,         
        hjust = 0,             
        direction = 'y',       
        box.padding = 0.5,     
        size = 4,
        show.legend = FALSE    
      ) +
      scale_y_reverse(
        breaks = function(limits) {
          # designed to both avoid places represented by decimals and '#0 place' from the dynamic y-axis
          
          min_axis <- max(1, ceiling(min(limits)))
          max_axis <- floor(max(limits))
          
          if (max_axis - min_axis <= 10) {
            # returns every integer if the range is <=10
            seq(min_axis, max_axis)  
          } else {
            # returns integers to avoid cramming the y-axis
            brks <- scales::extended_breaks(n = 6)(c(min_axis, max_axis))
            brks <- unique(round(brks))
            brks[brks >= min_axis & brks <= max_axis]
          }
        },
        labels = function(x) ifelse(x == min(x, na.rm = TRUE), paste0("#", x, '\nplace'), as.character(x))
      ) +
      scale_color_manual(values = color_values, labels = legend_labels) +
      labs(x = 'Split',
           y = NULL,
           title = "<b><span style='font-family:noto;font-size:18pt'>Place by Split</b></span>",
           subtitle = glue("<span style='font-family:noto;font-size:12pt'>Comparing the pace of <b><span style ='color:darkorange'>{test_athlete}</span></b> to runners either directly ahead or behind through <b>{dist_label}K</b></span>") 
      ) +
      scale_x_discrete(labels = function(x) paste0(as.numeric(x)/1000, "K")) +
      theme(legend.position = "top", axis.text.x = element_text(size = 10, color = 'gray50', family = 'ibm'),
            axis.text.y = element_text(size = 10, color = 'gray50', family = 'ibm'),
            axis.title.y.left = element_markdown(size = 10, family = 'noto', angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 11, family = 'noto'), legend.title = element_blank(),
            legend.text = element_text(family = 'noto'),
            plot.subtitle = ggtext::element_markdown(lineheight = 1.2),
            plot.title = ggtext::element_markdown(lineheight = 1.2),
            plot.title.position = "plot", axis.line = element_blank(), axis.ticks = element_blank(),
            panel.background = element_blank(), panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(), panel.grid.major.y = element_line(color = 'gray90', linetype = 'dashed'))
    
  }, res = 96)
  output$neighbor_pacing <- renderPlot({
    
    data <- adjacent_competitors()
    
    test_athlete <- data$athlete_var
    clean_xc_data    <- data$clean_xc_data 
    athlete_data_filtered <- data$athlete_data_filtered
    #athlete_filter <- data$athlete_filter
    up_until_distance <- as.integer(data$up_until_distance)
    neighbor_athletes <- data$neighbor_athletes
    
    neighbor_labels <- levels(athlete_data_filtered$labeler_label)[-1]  # everything except "Spotlighter"
    palette_colors <- c('#e08214', '#b2abd2', '#8073ac')[seq_len(length(neighbor_labels) + 1)]
    color_values <- setNames(palette_colors, c("Spotlighter", neighbor_labels))
    legend_labels <- setNames(c(test_athlete, neighbor_athletes), c("Spotlighter", neighbor_labels))
    
    dist_label <- up_until_distance/1000
    
    athlete_data_filtered |>
      ggplot() + 
      geom_vline(xintercept = factor(up_until_distance),
                 alpha = 0.6,
                 linewidth = 0.5) +
      geom_line(mapping = aes(x = factor(distance_m),
                              y = pace_km_min,
                              color = labeler_label,
                              group = athlete),
                linewidth = 1,
                #linetype = 'dashed',
                alpha = 0.5) +
      geom_line(athlete_data_filtered |> filter(distance_m <= up_until_distance),
                mapping = aes(x = factor(distance_m),
                              y = pace_km_min,
                              color = labeler_label,
                              group = athlete),
                linewidth = 1) +
      geom_point(athlete_data_filtered |> filter(distance_m <= up_until_distance),
                 mapping = aes(x = factor(distance_m),
                               y = pace_km_min,
                               color = labeler_label,
                               group = athlete),
                 size = 1.5) +
      geom_text_repel(
        data = athlete_data_filtered |> filter(distance_m == up_until_distance),
        mapping = aes(x = factor(distance_m),
                      y = pace_km_min,
                      label = format_times(pace_km_min),
                      color = labeler_label),
        fontface = 'bold',
        #nudge_x = 0.5,         
        hjust = 0,             
        direction = 'y',       
        box.padding = 0.5,     
        size = 4,
        show.legend = FALSE    
      ) +
      scale_color_manual(values = color_values, labels = legend_labels) +
      scale_y_reverse(
        
        labels = function(x) {
          ifelse(is.na(x), NA_character_,{
            formatted <- sprintf('%d:%02d',floor(x),round((x-floor(x))*60))
            
            ifelse(x == min(x,na.rm = T),
                   paste0(formatted,'\nper km'),
                   formatted)
            
          })
        }
        
      ) +
      scale_x_discrete(labels = function(x) paste0(as.numeric(x)/1000, "K")) +
      labs(
        x = NULL,
        y = NULL,
        title = "<b><span style='font-family:noto;font-size:18pt'>Pace per Km by Split</b></span>",
        subtitle = glue("<span style='font-family:noto;font-size:12pt'>Comparing the pace of <b><span style ='color:darkorange'>{test_athlete}</span></b> to runners either directly ahead or behind through <b>{dist_label}K</b></span>") 
      ) +
      theme(legend.position = "top", axis.text.x = element_text(size = 10, color = 'gray50', family = 'ibm'),
            axis.text.y = element_text(size = 10, color = 'gray50', family = 'ibm'),
            axis.title.y.left = element_markdown(size = 10, family = 'noto', angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 11, family = 'noto'), legend.title = element_blank(),
            legend.text = element_text(family = 'noto'),
            plot.subtitle = ggtext::element_markdown(lineheight = 1.2),
            plot.title = ggtext::element_markdown(lineheight = 1.2),
            plot.title.position = "plot", axis.line = element_blank(), axis.ticks = element_blank(),
            panel.background = element_blank(), panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(), panel.grid.major.y = element_line(color = 'gray90', linetype = 'dashed'))
    
    
  }, res = 96)
  output$neighbor_lead_size <- renderPlot({
    
    data <- adjacent_competitors()
    
    test_athlete <- data$athlete_var
    clean_xc_data    <- data$clean_xc_data 
    athlete_data_filtered <- data$athlete_data_filtered
    #athlete_filter <- data$athlete_filter
    up_until_distance <- as.integer(data$up_until_distance)
    neighbor_athletes <- data$neighbor_athletes
    
    neighbor_labels <- levels(athlete_data_filtered$labeler_label)[-1]  # everything except "Spotlighter"
    palette_colors <- c('#e08214', '#b2abd2', '#8073ac')[seq_len(length(neighbor_labels) + 1)]
    color_values <- setNames(palette_colors, c("Spotlighter", neighbor_labels))
    legend_labels <- setNames(c(test_athlete, neighbor_athletes), c("Spotlighter", neighbor_labels))
    
    dist_label <- up_until_distance/1000
    
    spotlighter_times <- athlete_data_filtered |>
      # cumulative times
      filter(labeler_label == 'Spotlighter') |>
      select(distance_m,
             spotlight_time = time_decimal)
    
    nonspot_times <- athlete_data_filtered |>
      # cumulative times
      filter(labeler_label != 'Spotlighter') |>
      select(athlete,
             labeler_label,
             distance_m,
             time_decimal) |>
      left_join(spotlighter_times) |>
      mutate(spotlight_lead = (spotlight_time-time_decimal)*60 )
    
    nonspot_times |>
      ggplot() +
      geom_hline(yintercept = 0,
                 linewidth = 0.5) +
      geom_vline(xintercept = factor(up_until_distance),
                 alpha = 0.6,
                 linewidth = 0.5) +
      geom_line(
        mapping = aes(x = factor(distance_m),
                      y = spotlight_lead,
                      color = labeler_label,
                      group = athlete),
        alpha = 0.5,
        linewidth = 1
      ) +
      geom_line(
        nonspot_times |> filter(distance_m <= up_until_distance),
        mapping = aes(x = factor(distance_m),
                      y = spotlight_lead,
                      color = labeler_label,
                      group = athlete),
        #smooth = 6,
        linewidth = 1
      ) +
      geom_point(nonspot_times |> filter(distance_m <= up_until_distance),
                 mapping = aes(x = factor(distance_m),
                               y = spotlight_lead,
                               color = labeler_label,
                               group = athlete),
                 size = 1.5) +
      scale_y_reverse(
        labels = function(x) {
          base <- scales::label_number(style_positive = "plus", accuracy = 1)(x)
          #ifelse(x == min(x, na.rm = TRUE), paste0(base, "\nsecs"), base)
          
          case_when(
            x == min(x, na.rm = TRUE) & x < 0 ~ paste0(base, "\nsec\nlead"),
            x == min(x, na.rm = TRUE) ~ paste0(base, "\nsecs"),
            x == 0 ~ stringr::str_wrap( glue('{test_athlete} race time')),
            TRUE ~ base
          )
          
        }
      ) +
      labs(
        x = NULL,
        y = NULL
      ) +
      scale_x_discrete(labels = function(x) paste0(as.numeric(x)/1000, "K")) +
      scale_color_manual(values = color_values, labels = legend_labels) +
      theme(legend.position = "top", axis.text.x = element_text(size = 10, color = 'gray50', family = 'ibm'),
            axis.text.y = element_text(size = 10, color = 'gray50', family = 'ibm'),
            axis.title.y.left = element_markdown(size = 10, family = 'noto', angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 11, family = 'noto'), legend.title = element_blank(),
            legend.text = element_text(family = 'noto'),
            plot.subtitle = ggtext::element_markdown(lineheight = 1.2),
            plot.title = ggtext::element_markdown(lineheight = 1.2),
            plot.title.position = "plot", axis.line = element_blank(), axis.ticks = element_blank(),
            panel.background = element_blank(), panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(), panel.grid.major.y = element_line(color = 'gray90', linetype = 'dashed'))
    
  
    }, res = 96)
}
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))