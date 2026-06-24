library(shiny)
library(ggplot2)
library(ggbeeswarm)
library(glue)
library(colorspace)
library(showtext)
library(tidyverse)
library(shadowtext)
library(ggtext)
library(ggpattern)

source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251207 XC Data Cleaning Functions.R')
source('~/GitHub/Sports/College Cross Country/20251118 MW Regionals/20251129 school colors.R')

# ============================================================================
# SETUP - SELECT EVENT & ATHLETE
# ============================================================================

event_var <- "NCAA DI Midwest Regional Women's 6K"

athlete_var <- full_xc_df |>
  filter(event == event_var) |>
  distinct(athlete) |>
  sample_n(1) |>
  pull(athlete)

school_var <- full_xc_df |>
  filter(event == event_var, athlete == athlete_var) |>
  pull(school) |>
  first()

school_color_var <- school_colors$primary_color[school_colors$school == school_var][1]
school_color_var_light <- lighten(school_color_var, 0.5)

# ============================================================================
# DATA PREP - FINAL SPLIT TIMES (Charts 1 & 2)
# ============================================================================

final_split_times <- full_xc_df |>
  filter(event == event_var, split == max(split[event == event_var])) |>
  select(school, athlete, time_decimal, place) |>
  mutate(
    color_spotlight = factor(
      case_when(
        athlete == athlete_var ~ 'spotlight',
        school == school_var ~ 'teammates',
        TRUE ~ 'other'
      ),
      levels = c('spotlight','teammates','other')
    )
  )

# Pre-filter subsets (reused across both charts)
data_other <- filter(final_split_times, color_spotlight == 'other')
data_teammates <- filter(final_split_times, color_spotlight == 'teammates')
data_spotlight <- filter(final_split_times, color_spotlight == 'spotlight')
athlete_time <- data_spotlight$time_decimal[1]

# Calculate medians
group_medians <- final_split_times |>
  filter(!is.na(time_decimal)) |>
  mutate(
    group_for_median = if_else(color_spotlight == "spotlight", "teammates", as.character(color_spotlight))
  ) |>
  group_by(group_for_median) |>
  summarise(
    median_time = median(time_decimal, na.rm = TRUE),
    y_position = max(density(time_decimal)$y),
    .groups = "drop"
  ) |>
  transmute(
    color_spotlight = factor(group_for_median, levels = c('teammates', 'other')),
    median_time,
    y_position,
    label = if_else(group_for_median == "teammates", school_var, "Others")
  )

teammates_median <- group_medians$median_time[1]
others_median <- group_medians$median_time[2]

# Format times & calculate differences
athlete_time_formatted <- decimal_to_time(athlete_time)
teammates_median_formatted <- decimal_to_time(teammates_median)
others_median_formatted <- decimal_to_time(others_median)

diff_vs_team <- athlete_time - teammates_median
team_diff_text <- case_when(
  abs(diff_vs_team * 60) <= 1 ~ "around the same time as",
  diff_vs_team < 0 ~ paste0(format_time_diff(diff_vs_team * 60), " faster than "),
  TRUE ~ paste0(format_time_diff(diff_vs_team * 60), " slower than ")
)

# Plot limits (reused)
min_time <- floor(min(final_split_times$time_decimal, na.rm = TRUE))
max_time <- ceiling(max(final_split_times$time_decimal, na.rm = TRUE))

# ============================================================================
# DATA PREP - PACING STATS (Chart 3)
# ============================================================================
dnf_runners <- full_xc_df |>
  filter(split == max(split), is.na(time_decimal)) |>
  pull(athlete)

event_pacing_stats <- full_xc_df |>
  filter(event == event_var, !athlete %in% dnf_runners) |>
  arrange(athlete, split) |>
  group_by(athlete, school, class) |>
  mutate(
    pace_per_km = (time_decimal - lag(time_decimal)) * 60 / ((split - lag(split)) / 1000)
  ) |>
  ungroup() |>
  filter(!is.na(pace_per_km))

# Group summaries for all three groups
group_summary_splits <- event_pacing_stats |>
  mutate(
    color_spotlight = factor(
      case_when(
        athlete == athlete_var ~ 'spotlight',
        school == school_var ~ 'teammates',
        TRUE ~ 'other'
      ),
      levels = c('spotlight','teammates','other')
    )
  ) |>
  group_by(split, color_spotlight) |>
  summarise(median_pace_km = median(pace_per_km, na.rm = TRUE), .groups = 'drop') |>
  filter(split != min(split))  # Remove first split

# Top 25 finishers range
top_25_finishers <- event_pacing_stats |>
  filter(split == max(split), place <= 25) |>
  pull(athlete)

top_25_range_pacing <- event_pacing_stats |>
  filter(athlete %in% top_25_finishers) |>
  group_by(split) |>
  summarise(
    slowest_pace = min(pace_per_km, na.rm = TRUE),
    fastest_pace = max(pace_per_km, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  filter(!is.infinite(slowest_pace), split != min(split))

# Full field range
range_pacing <- event_pacing_stats |>
  group_by(split) |>
  summarise(
    slowest_pace = min(pace_per_km, na.rm = TRUE),
    fastest_pace = max(pace_per_km, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  filter(!is.infinite(slowest_pace), split != min(split))

# Find slowdown split (before converting to factor)
slowdown_split_value <- range_pacing |>
  slice_max(slowest_pace, n = 1) |>
  pull(split)

# Get max split value for annotations
max_split_value <- max(top_25_range_pacing$split)
top_25_range_lastk <- filter(top_25_range_pacing, split == max_split_value)

# Convert to factors for plotting
plot_data <- mutate(group_summary_splits, split = factor(split))
range_pacing <- mutate(range_pacing, split = factor(split))
top_25_range_pacing <- mutate(top_25_range_pacing, split = factor(split))

# Dynamic positioning
n_splits <- length(unique(plot_data$split))
last_split_position <- n_splits
min_range <- floor(min(as.numeric(as.character(range_pacing$slowest_pace)))) - 4
max_range <- round(max(as.numeric(as.character(range_pacing$fastest_pace)))) + 1
slowdown_position <- which(as.numeric(levels(range_pacing$split)) == slowdown_split_value)

# ============================================================================
# CHART 1 - DOT PLOT
# ============================================================================

ggplot(final_split_times, aes(x = 0, y = time_decimal)) + 
  geom_quasirandom(data = data_other, shape = 21, stroke = 1, width = 0.4, method = 'smiley',
                   fill = 'gray80', color = 'white', size = 4, alpha = 0.6) +
  geom_hline(data = filter(group_medians, color_spotlight == 'other'),
             aes(yintercept = median_time), color = 'gray70', linewidth = 1.2) +
  geom_hline(data = filter(group_medians, color_spotlight == 'teammates'),
             aes(yintercept = median_time), color = 'white', linewidth = 1.8) +
  geom_hline(data = filter(group_medians, color_spotlight == 'teammates'),
             aes(yintercept = median_time), color = school_color_var_light, linewidth = 1.2) +
  geom_quasirandom(data = data_teammates, shape = 21, stroke = 1, width = 0.4, method = 'smiley',
                   fill = school_color_var, color = 'white', size = 6, alpha = 0.8) +
  geom_hline(yintercept = athlete_time, color = 'white', linewidth = 1.8) +
  geom_hline(yintercept = athlete_time, color = school_color_var, linewidth = 1.2) +
  geom_quasirandom(data = data_spotlight, aes(fill = color_spotlight, color = color_spotlight == "spotlight"),
                   shape = 21, stroke = 0.7, width = 0.4, method = 'smiley', size = 6) +
  geom_shadowtext(data = data_spotlight, aes(label = athlete),
                  position = position_quasirandom(width = 0.4, method = 'smiley'),
                  size = 4, fontface = "bold", family = 'noto', hjust = 1.2, 
                  bg.color = 'black', color = school_color_var_light) +
  coord_flip() +
  scale_y_reverse(limits = c(max_time, min_time), breaks = seq(min_time, max_time, by = 1.5),
                  labels = function(x) sprintf("%d:%02d", floor(x), round((x - floor(x)) * 60))) +
  scale_color_manual(values = c('TRUE' = 'black', 'FALSE' = 'white'), guide = 'none') +
  scale_alpha_manual(values = c("spotlight" = 1, "teammates" = 1, "other" = 0.6), guide = 'none') +
  scale_fill_manual(values = c("spotlight" = school_color_var, "teammates" = school_color_var_light, "other" = "gray85"), guide = 'none') +
  labs(x = '', y = 'Race Time\n← Slower                    Faster →',
       caption = '\nVisualizations by Alex Elfering\nSource: NCAA & PrimeTime Timing') +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_blank(),
        axis.text.x = element_text(size = 13, color = 'gray80', family = 'ibm'),
        axis.title.x = element_text(size = 13, face = 'bold', family = 'noto'),
        plot.caption = element_text(size = 13, family = 'noto', hjust = 0, color = 'gray80'),
        axis.title.y = element_blank(), axis.line = element_blank(), axis.ticks = element_blank(),
        panel.background = element_blank(), panel.grid = element_blank())

# ============================================================================
# CHART 2 - DENSITY
# ============================================================================

ggplot(final_split_times, aes(x = time_decimal, color = color_spotlight, fill = color_spotlight)) +
  geom_density(data = data_other, alpha = 0.5, linewidth = 1) +
  geom_density(data = data_teammates, alpha = 0.5, linewidth = 1) +
  geom_errorbar(data = data_spotlight, aes(xmin = time_decimal - 0.2, xmax = time_decimal + 0.2, ymin = 0, ymax = 0)) +
  geom_point(data = data_spotlight, aes(y = 0), size = 4) +
  scale_x_reverse(limits = c(max_time, min_time), breaks = seq(min_time, max_time, by = 1.5)) +
  geom_shadowtext(data = group_medians, aes(x = median_time, y = y_position, label = label),
                  size = 4, fontface = "bold", family = "noto") +
  scale_fill_manual(values = c("spotlight" = school_color_var, "teammates" = school_color_var_light, "other" = "gray85"), guide = 'none') +
  scale_color_manual(values = c("spotlight" = school_color_var, "teammates" = school_color_var_light, "other" = "gray70"), guide = 'none') +
  labs(title = glue("<b><span style='font-size:18pt'>{event_var}</span></b>"),
       subtitle = glue(
         "<b><span style='font-size:12pt; color:{school_color_var};'>{athlete_var}</span></b> finished {team_diff_text} ",
         "the <b><span style='font-size:12pt; color:{school_color_var};'>Median {school_var}</span></b> time at ",
         "<span style='font-family:ibm; font-weight:bold;'>{athlete_time_formatted}</span>. ",
         "<br>The median <b><span style='font-size:12pt; color:{school_color_var};'>{school_var}</span></b> runner finished with a time of ",
         "<span style='font-family:ibm; font-weight:bold;'>{teammates_median_formatted}</span>, while <b><span style='font-size:12pt; color:gray70;'>the rest of the field</span></b> finished with a median time of ",
         "<span style='font-family:ibm; font-weight:bold;'>{others_median_formatted}</span>."
       )) +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 14, family = "noto", lineheight = 1.2, margin = margin(b = 5)),
        plot.subtitle = element_markdown(size = 11, family = "noto", lineheight = 1.3, color = "gray30"),
        axis.text = element_blank(), axis.title = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(), panel.grid = element_blank())

# ============================================================================
# CHART 3 - PACING
# ============================================================================

ggplot(range_pacing) + 
  geom_ribbon(aes(x = split, ymin = slowest_pace, ymax = fastest_pace, group = 1),
              fill = 'gray80', alpha = 0.4) +
  geom_ribbon_pattern(data = top_25_range_pacing,
                      aes(x = split, ymin = slowest_pace, ymax = fastest_pace, group = 1), 
                      fill = "gray60", pattern_color = "white", pattern_fill = "white",
                      pattern_angle = 135, pattern_density = 0.25, pattern_spacing = 0.01,
                      pattern_key_scale_factor = 0.5, alpha = 0.4) +
  geom_line(data = filter(plot_data, color_spotlight == 'other'),
            aes(x = split, y = median_pace_km, color = color_spotlight, group = color_spotlight), linewidth = 1.5) +
  geom_line(data = filter(plot_data, color_spotlight == 'teammates'),
            aes(x = split, y = median_pace_km, group = color_spotlight), color = 'white', linewidth = 2.5) +
  geom_line(data = filter(plot_data, color_spotlight == 'teammates'),
            aes(x = split, y = median_pace_km, color = color_spotlight, group = color_spotlight), linewidth = 1.5) +
  geom_point(data = filter(plot_data, color_spotlight == 'spotlight'),
             aes(x = split, y = median_pace_km), color = 'white', size = 2.5) +
  geom_line(data = filter(plot_data, color_spotlight == 'spotlight'),
            aes(x = split, y = median_pace_km, group = color_spotlight), color = 'white', linewidth = 2.5) +
  geom_line(data = filter(plot_data, color_spotlight == 'spotlight'),
            aes(x = split, y = median_pace_km, color = color_spotlight, group = color_spotlight), linewidth = 1.5) +
  geom_point(data = filter(plot_data, color_spotlight == 'spotlight'),
             aes(x = split, y = median_pace_km, color = color_spotlight), size = 1.5) +
  scale_color_manual(values = c("spotlight" = school_color_var, "teammates" = school_color_var_light, "other" = "gray50"),
                     labels = c("teammates" = glue("{school_var}"), "other" = "Rest of the Field")) +
  scale_y_continuous(limits = c(min_range, max_range), breaks = seq(min_range, max_range, by = 15),
                     labels = function(x) ifelse(x == max(x), paste0(x, " Seconds\nper km"), as.character(x))) +
  scale_x_discrete(labels = function(x) paste0(as.numeric(x)/1000, "K")) +
  annotate("segment", x = last_split_position + 0.025, xend = last_split_position + 0.025,
           y = top_25_range_lastk$slowest_pace, yend = top_25_range_lastk$fastest_pace,
           linetype = "dotted", color = "gray50", linewidth = 0.5) +
  annotate("text", x = last_split_position + 0.04,
           y = round((top_25_range_lastk$slowest_pace + top_25_range_lastk$fastest_pace)/2),
           label = "Top 25\nrunners\nstayed\nin this\nrange", size = 3, family = 'noto', hjust = 0, fontface = 'bold') +
  annotate("segment", x = 2.54, xend = 3, y = max_range - 3, yend = max_range - 3,
           linetype = "dotted", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 2.5, y = max_range - 3, label = "Slowest pace",
           size = 3, family = 'noto', hjust = 1, fontface = 'bold') +
  labs(title = glue("<b><span style='font-size:18pt'>{event_var}</span></b>"),
       x = '\nSplit', y = 'Slower ▲\n\n\n\n\n\n\n\n\nFaster ▼') +
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(size = 10, color = 'gray50', family = 'ibm'),
        axis.text.y = element_text(size = 10, color = 'gray50', family = 'ibm'),
        axis.title.y.left = element_text(size = 10, family = 'noto', angle = 0, vjust = 0.5),
        axis.title.x = element_text(size = 11, family = 'noto'), legend.title = element_blank(),
        legend.text = element_text(family = 'noto'),
        plot.title = element_markdown(size = 14, family = "noto", lineheight = 1.2, margin = margin(b = 5)),
        plot.title.position = "plot", axis.line = element_blank(), axis.ticks = element_blank(),
        panel.background = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.grid.major.y = element_line(color = 'gray90', linetype = 'dashed'))