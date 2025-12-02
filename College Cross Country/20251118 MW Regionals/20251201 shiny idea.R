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

font_add_google("IBM Plex Sans", "ibm")
font_add_google("Noto Sans", "noto")

# Helper function to convert decimal to time
decimal_to_time <- function(x) {
  mins <- floor(x)
  secs <- round((x - mins) * 60, 1)
  sprintf("%d:%05.2f", mins, secs)
}

# Get unique athletes and schools
athlete_list <- mens_mw_clean |> 
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
      
      # Top N slider
      sliderInput(
        "top_n",
        "Compare to Top N Finishers:",
        min = 5,
        max = 50,
        value = 10,
        step = 5
      ),
      
      hr(),
      
      # Summary stats
      h4("Selected Athlete Stats:"),
      tableOutput("athlete_stats"),
      
      hr(),
      
      downloadButton("download_plot", "Download Plot")
    ),
    
    mainPanel(
      width = 9,
      
      # Density plot on top
      plotOutput("density_plot", height = "250px"),
      
      # Main quasi-random plot below
      plotOutput("quasi_plot", height = "500px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values based on selected athlete
  athlete_data <- reactive({
    req(input$athlete)
    
    athlete_var <- input$athlete
    top_n <- input$top_n
    
    school_var <- mens_mw_clean |>
      filter(athlete == athlete_var) |>
      distinct(school) |>
      pull(school)
    
    teammates_10k_var <- mens_mw_clean |>
      filter(school == school_var, split == 10000) |>
      pull(athlete)
    
    top_n_10k_var <- mens_mw_clean |>
      filter(split == 10000) |>
      filter(dense_rank(time_decimal) <= top_n) |>
      pull(athlete)
    
    school_color_var <- school_colors |>
      filter(school == school_var) |>
      pull(primary_color)
    
    school_color_var_light <- lighten(school_color_var, amount = 0.5)
    
    mens_10k_times <- mens_mw_clean |>
      filter(split == 10000) |>
      select(school, athlete, time_decimal, place) |>
      mutate(
        color_spotlight = case_when(
          athlete == athlete_var ~ 'spotlight',
          school == school_var ~ 'teammates',
          #athlete %in% top_n_10k_var ~ 'top_n',
          TRUE ~ 'other'
        ),
        color_spotlight = factor(color_spotlight, 
                                 levels = c('spotlight','teammates','other'))
      )
    
    athlete_time <- mens_10k_times |>
      filter(athlete == athlete_var) |>
      pull(time_decimal)
    
    list(
      athlete_var = athlete_var,
      school_var = school_var,
      top_n = top_n,
      school_color_var = school_color_var,
      school_color_var_light = school_color_var_light,
      mens_10k_times = mens_10k_times,
      athlete_time = athlete_time
    )
  })
  
  # Quasi-random plot
  output$quasi_plot <- renderPlot({
    data <- athlete_data()
    
    #set.seed(42)  # ADD THIS - ensures consistent quasirandom positions
    
    min_time <- floor(min(data$mens_10k_times$time_decimal, na.rm = TRUE))
    max_time <- round(max(data$mens_10k_times$time_decimal, na.rm = TRUE)) + 1
    
    athlete_time_value <- data$mens_10k_times |>
      filter(athlete == data$athlete_var) |>
      pull(time_decimal)
    
    data$mens_10k_times |>
      ggplot(aes(x = 0, y = time_decimal)) + 
      geom_quasirandom(
        data = data$mens_10k_times |> filter(athlete != data$athlete_var),
        aes(fill = color_spotlight,
            color = color_spotlight == "spotlight",
            alpha = color_spotlight,
            size = color_spotlight),
        shape = 21,
        stroke = 1,
        width = 0.4,
        method = 'smiley',
        size = 6
      ) +
      geom_hline(
        yintercept = athlete_time_value,
        color = 'white',
        linewidth = 1.5,
        #linetype = 'dashed',
        alpha = 0.7
      ) +
      geom_hline(
        yintercept = athlete_time_value,
        color = data$school_color_var,
        linewidth = 1.2,
        #linetype = 'dashed',
        alpha = 0.7
      ) +
      geom_quasirandom(
        data = data$mens_10k_times |> filter(athlete == data$athlete_var),
        aes(fill = color_spotlight,
            color = color_spotlight == "spotlight",
            alpha = color_spotlight,
            size = color_spotlight),
        shape = 21,
        stroke = 1,
        width = 0.4,
        method = 'smiley',
        size = 6
      ) +
      geom_shadowtext(
        data = data$mens_10k_times |> filter(athlete == data$athlete_var),
        aes(label = glue('{athlete}: {decimal_to_time(time_decimal)}')),
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
        limits = rev(c(min_time, max_time)),
        breaks = seq(min_time, max_time, by = 1.5),
        labels = function(x) {
          # FORMAT LABELS AS mm:ss
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
        values = c("spotlight" = 1, "teammates" = 1, "top_n" = 0.8, "other" = 0.6)
      ) +
      #scale_size_manual(
      #  values = c("spotlight" = 5, "teammates" = 4, "top_n" = 3, "other" = 2.5)
      #) +
      scale_fill_manual(
        values = c(
          "spotlight" = data$school_color_var,
          "teammates" = data$school_color_var_light,
          #"top_n" = "gray30",
          "other" = "gray85"
        ),
        labels = c(
          "spotlight" = data$athlete_var,
          "teammates" = glue("{data$school_var}"),
          #"top_n" = glue("Top {data$top_n} Finishers"),
          "other" = "Other runners"
        )
      ) +
      labs(
        #x = '',
        y = '10K Race Time\n← Slower                                                     Faster →',
      #  title = glue("{data$athlete_var} ({data$school_var}) Performance Distribution")
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.caption = element_text(size = 8, hjust = 0, color = 'gray80'),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, hjust = 0, color = 'gray80'),
        axis.title.y = element_text(angle = 90,, hjust = 1, vjust = 1),
        #axis.text.x = element_blank(),
        legend.title = element_blank(),
        plot.title.position = "plot", 
        plot.caption.position = 'plot',
        axis.line.y.left = element_line(color = 'gray50'),
        axis.line.x.bottom = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_line(color = 'gray90', linetype = 'dashed')
      )
    
  }, res = 96)
  
  # Density plot
  output$density_plot <- renderPlot({
    data <- athlete_data()
    
    min_time <- floor(min(data$mens_10k_times$time_decimal, na.rm = TRUE))
    max_time <- round(max(data$mens_10k_times$time_decimal, na.rm = TRUE)) + 1
    
    # Calculate medians for each group
    group_medians <- data$mens_10k_times |>
      filter(color_spotlight != 'spotlight',
             !is.na(time_decimal)) |>
      group_by(color_spotlight) |>
      summarise(
        median_time = median(time_decimal, na.rm = TRUE),
        y_position = max((density(time_decimal))$y),
        .groups = "drop"
      ) |>
      arrange(median_time) |>
      mutate(
        label = case_when(
          #color_spotlight == "spotlight" ~ glue("{data$athlete_var}: {decimal_to_time(median_time)}"),
          color_spotlight == "teammates" ~ glue("{data$school_var}: {decimal_to_time(median_time)}"),
          #color_spotlight == "top_n" ~ glue("Top {data$top_n}: {decimal_to_time(median_time)}"),
          color_spotlight == "other" ~ glue("Others: {decimal_to_time(median_time)}")
        ),
        # Stagger y positions to avoid overlap
        y_label = seq(0.05, 0.2, length.out = n())
      )
    
    athlete_time_value <- data$mens_10k_times |>
      filter(athlete == data$athlete_var) |>
      pull(time_decimal)
    
    data$mens_10k_times |>
      ggplot( aes(x=time_decimal, color=color_spotlight, fill=color_spotlight)) +
      geom_density(alpha=0.6) +
      scale_x_reverse(
        limits = rev(c(min_time, max_time)),
        breaks = seq(min_time, max_time, by = 1.5)
      ) +
      geom_vline(
        data = athlete_time_value,
        aes(xintercept = time_decimal, color = color_spotlight),
        linetype = "solid",
        linewidth = 0.8
      ) +
      geom_vline(
        data = group_medians,
        aes(xintercept = median_time, color = color_spotlight),
        linetype = "dashed",
        linewidth = 0.8
      ) +
      geom_shadowtext(
        data = group_medians,
        aes(x = median_time, y = y_position, label = label, color = color_spotlight),
        size = 4,
        fontface = "bold",
        family = "noto",
      ) +
      # Add lab
      scale_fill_manual(
        values = c(
          "spotlight" = data$school_color_var,
          "teammates" = data$school_color_var_light,
          "other" = "gray85"
        ),
        labels = c(
          "spotlight" = data$athlete_var,
          "teammates" = glue("{data$school_var}"),
          "other" = "Other runners"
        )
      ) +
      scale_color_manual(
        values = c(
          "spotlight" = data$school_color_var,
          "teammates" = data$school_color_var_light,
          "top_n" = "gray30",
          "other" = "gray85"
        ),
        labels = c(
          "spotlight" = data$athlete_var,
          "teammates" = glue("{data$school_var}"),
          "other" = "Other runners"
        )
      ) +
      theme(
        legend.position = "none",
        plot.caption = element_text(size = 8, hjust = 0, color = 'gray80',family = 'noto'),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title.position = "plot", 
        plot.caption.position = 'plot',
        axis.line.y.left = element_line(color = 'gray50'),
        axis.line.x.bottom = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_line(color = 'gray90', linetype = 'dashed')
      )
  }, res = 96)
  
  # Athlete stats table
  output$athlete_stats <- renderTable({
    data <- athlete_data()
    
    athlete_info <- data$mens_10k_times |>
      filter(athlete == data$athlete_var) |>
      select(place, time_decimal)
    
    team_avg <- data$mens_10k_times |>
      filter(school == data$school_var) |>
      summarise(avg = mean(time_decimal, na.rm = TRUE)) |>
      pull(avg)
    
    data.frame(
      Metric = c("Place", "Time", "School Avg", "Diff from Avg"),
      Value = c(
        as.character(athlete_info$place),
        decimal_to_time(athlete_info$time_decimal),
        decimal_to_time(team_avg),
        decimal_to_time(athlete_info$time_decimal - team_avg)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
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