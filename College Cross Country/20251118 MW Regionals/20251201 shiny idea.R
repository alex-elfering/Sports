# Shiny App: NCAA Midwest Regional Athlete Comparison
# Load libraries and data

library(shiny)
library(ggplot2)
library(ggbeeswarm)
library(glue)
library(colorspace)
library(showtext)
library(dplyr)
library(shadowtext)

# Load your data sources
source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251118 Mens MW Regional Data Cleaning.R')
source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251129 school colors.R')

#showtext_auto()
#showtext_opts(dpi = 96)  # Use 96 for screen display

font_add_google("Fira Mono", "ibm")
font_add_google("Noto Sans", "noto")

# Helper function to convert decimal to time
decimal_to_time <- function(x) {
  mins <- floor(x)
  secs <- round((x - mins) * 60, 1)
  sprintf("%d:%05.2f", mins, secs)
}

# Get unique athletes and schools
athlete_list <- mens_mw_clean |> 
  filter(split == 10000, !is.na(time_decimal)) |>  # Only finishers at 10k
  distinct(athlete, school) |> 
  arrange(school, athlete)

# UI
ui <- fluidPage(
  
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("NCAA Midwest Regional 10K - Athlete Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Athlete selector - starts with random athlete
      selectInput(
        "athlete",
        "Select Athlete:",
        choices = setNames(sort(athlete_list$athlete), 
                           sort(paste0(athlete_list$athlete, " (", athlete_list$school, ")"))),
        selected = sample(athlete_list$athlete, 1)  # RANDOM SELECTION ON LOAD
      ),
      
      hr(),
      
      downloadButton("download_plot", "Download Plot")
    ),
    
    mainPanel(
      width = 9,
      
      # Density plot on top
      plotOutput("density_plot", height = "150px",width = "800px"),
      
      #hr(),
      # Main quasi-random plot below
      plotOutput("quasi_plot", height = "300px",width = "800px")
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
      distinct(school) |>
      pull(school)
    
    school_color_var <- school_colors |>
      filter(school == school_var) |>
      pull(primary_color)
    
    school_color_var_light <- lighten(school_color_var, amount = 0.5)
    
    # Filter to 10k split ONCE instead of multiple times
    mens_10k_times <- mens_mw_clean |>
      filter(split == 10000) |>
      select(school, athlete, time_decimal, place) |>
      mutate(
        color_spotlight = case_when(
          athlete == athlete_var ~ 'spotlight',
          school == school_var ~ 'teammates',
          TRUE ~ 'other'
        ),
        color_spotlight = factor(color_spotlight, 
                                 levels = c('spotlight','teammates','other'))
      )
    
    athlete_time <- mens_10k_times$time_decimal[mens_10k_times$athlete == athlete_var]  # Faster than filter + pull
    
    # Pre-calculate group medians ONCE (used in both plots)
    group_medians <- mens_10k_times |>
      filter(color_spotlight != 'spotlight', !is.na(time_decimal)) |>
      group_by(color_spotlight) |>
      summarise(
        median_time = median(time_decimal, na.rm = TRUE),
        y_position = max(density(time_decimal)$y),
        .groups = "drop"
      ) |>
      arrange(median_time) |>
      mutate(
        label = if_else(color_spotlight == "teammates", 
                        school_var, 
                        "Others")  # if_else is faster than case_when for binary
      )
    
    # use for calculating pace between splits
    
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
    
    split_pace_per_km <- mens_mw_clean |>
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
    
    # Pre-filter data subsets (avoid filtering in each geom)
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
      data_spotlight = data_spotlight
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
        bg.color = 'white',
        color = data$school_color_var
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
        y = '10K Race Time\n← Slower                                                     Faster →'
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 13, color = 'gray80', family = 'ibm'),
        axis.title.x = element_text(size = 13, face = 'bold', family = 'ibm'),
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
    
    ggplot(data$mens_10k_times, aes(x = time_decimal, color = color_spotlight, fill = color_spotlight)) +
      
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
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank()
      )
    
  }, res = 96)
  
  # Download handler
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", athlete_data()$athlete_var), "_comparison.png")
    },
    content = function(file) {
      # Re-render the plot for download at higher resolution
      ggsave(file, plot = last_plot(), width = 10, height = 7, dpi = 300)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)