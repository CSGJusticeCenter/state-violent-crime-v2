## graphics for 2024 ucr update
## request from jess: I'm thinking four graphics in total: (1) violent crime rate 2019-2024, (2) violent crime rate 2023-2024, (3) violent crime solve rates 2019-2024, (4) violent crime solve rates 2023-2024.

library(tidyverse)
library(csgjcr)
library(scales)

srs_state_new <- read_rds(csg_sp_path("jr_data_library", "data", "analysis", "fbi", "srs", "fbi_srs_state.rds")) |> 
  mutate(crime_cat = if_else(group %in% c("Aggravated assault", "Homicide", "Rape", "Robbery"), "Violent", "Property")) |> 
  select(-state_fips, -group_cat, -starts_with("rate")) |> 
  filter(indicator != "Unsolved rate") |> 
  pivot_wider(names_from = indicator, values_from = n) |> 
  janitor::clean_names() |> 
  select(
    year, state_name, state_abbr, group, crime_cat, pop_total, pop_adult,
    incidents_reported, incidents_unsolved, incidents_cleared) |> 
  mutate(
    pct_unsolved = incidents_unsolved / incidents_reported,
    incidents_reported_rate_total = incidents_reported / pop_total,
    incidents_reported_rate_adult = incidents_reported / pop_adult
  )


viol_crime_state <- srs_state_new |> 
  filter(year >= 2019, crime_cat == "Violent") |> 
  group_by(year, state_name, state_abbr) |> 
  summarize(
    incidents_reported = sum(incidents_reported, na.rm = TRUE),
    incidents_unsolved = sum(incidents_unsolved, na.rm = TRUE),
    incidents_cleared = sum(incidents_cleared, na.rm = TRUE),
    pop_total = sum(pop_total, na.rm = TRUE) / 4,
    .groups = "drop"
  ) |> 
  mutate(
    incidents_per_100k = incidents_reported / pop_total * 100000,
    solve_rate = incidents_cleared / incidents_reported
  )

viol_crime_us <- viol_crime_state |> 
  group_by(year, state_name = "United States Total", state_abbr = "U.S.") |> 
  summarize(
    incidents_reported = sum(incidents_reported, na.rm = TRUE),
    incidents_unsolved = sum(incidents_unsolved, na.rm = TRUE),
    incidents_cleared = sum(incidents_cleared, na.rm = TRUE),
    pop_total = sum(pop_total, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(
    incidents_per_100k = incidents_reported / pop_total * 100000,
    solve_rate = incidents_cleared / incidents_reported
  )

to_plot <- viol_crime_state |> 
  bind_rows(viol_crime_us) |>
  filter(year %in% c(2019, 2024)) |> 
  select(year, state_name, incidents_per_100k) |> 
  pivot_wider(names_from = year, values_from = incidents_per_100k, names_prefix = "yr_") |> 
  mutate(
    change = yr_2024 - yr_2019,
    change_pct = (yr_2024 - yr_2019) / yr_2019,
    direction = if_else(change > 0, "Increased", "Decreased")
  ) |>
  # Reorder states by 2019 rate for better visual organization
  mutate(
    state_name = fct_reorder(state_name, yr_2024),
    state_name = fct_relevel(state_name, "United States Total", after = 51)
    )

# arrow plot of violent crime rate changes 2019 to 2024 by state# Create arrow plot showing violent crime rate changes 2019 to 2024
to_plot |>
  ggplot(aes(y = state_name)) +
  # Add light blue background for United States row
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 51.5, ymax = 52.5, 
           fill = "lightblue", alpha = 0.3) +
  geom_hline(yintercept = seq(0.5, 52.5, 1), color = "gray90", linewidth = 0.5) +
  # Draw arrows from 2019 to 2024 values
  geom_segment(aes(x = yr_2019, xend = yr_2024, color = direction),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = 1.5) +
  # Add rate values at arrow ends - adjusted positioning
  geom_text(data = filter(to_plot, direction == "Increased"), 
            aes(x = yr_2019, label = comma(yr_2019, 1)),
            hjust = 1, size = 3, nudge_x = -15, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Decreased"), 
            aes(x = yr_2019, label = comma(yr_2019, 1)),
            hjust = 1, size = 3, nudge_x = 55, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Increased"), 
            aes(x = yr_2024, label = comma(yr_2024, 1)),
            hjust = 1, size = 3, nudge_x = 55, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Decreased"), 
            aes(x = yr_2024, label = comma(yr_2024, 1)),
            hjust = 1, size = 3, nudge_x = -15, color = "gray20", family = "Inter") +
  geom_text(
    aes(x = 1200, label = percent(change_pct, accuracy = 1, style_positive = "plus",
    style_negative = "minus")),
    hjust = 1, size = 3, color = "gray20", family = "Inter"
  ) +
  # Use blue for decreases, purple for increases to match your reference
  scale_color_manual(values = c("Decreased" = "#4095B1", "Increased" = "#8B5A9C")) +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.0)), labels = label_comma(),
  breaks = c(0, 250, 500, 750, 1000)) +
  labs(
    title = "Change in Violent Crime, 2019 to 2024",
    subtitle = "Violent crime reported to police per 100,000 residents",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: FBI Uniform Crime Reporting Program"
  ) +
  theme_minimal(base_family = "Inter") +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 9, face = "bold", family = "Inter"),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 10, family = "Inter"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", family = "Inter"),
    plot.subtitle = element_text(family = "Inter"),
    plot.caption = element_text(size = 8, family = "Inter"),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("violent_crime_rate_change_2019_2024.png", width = 8, height = 10, dpi = 300, bg = "white")


to_plot <- viol_crime_state |> 
  bind_rows(viol_crime_us) |>
  filter(year %in% c(2023, 2024)) |> 
  select(year, state_name, incidents_per_100k) |> 
  pivot_wider(names_from = year, values_from = incidents_per_100k, names_prefix = "yr_") |> 
  mutate(
    change = yr_2024 - yr_2023,
    change_pct = (yr_2024 - yr_2023) / yr_2023,
    direction = if_else(change > 0, "Increased", "Decreased")
  ) |>
  # Reorder states by 2023 rate for better visual organization
  mutate(
    state_name = fct_reorder(state_name, yr_2024),
    state_name = fct_relevel(state_name, "United States Total", after = 51)
    )

# arrow plot of violent crime rate changes 2023 to 2024 by state# Create arrow plot showing violent crime rate changes 2023 to 2024
to_plot |>
  ggplot(aes(y = state_name)) +
  # Add light blue background for United States row
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 51.5, ymax = 52.5, 
           fill = "lightblue", alpha = 0.3) +
  geom_hline(yintercept = seq(0.5, 52.5, 1), color = "gray90", linewidth = 0.5) +
  # Draw arrows from 2023 to 2024 values
  geom_segment(aes(x = yr_2023, xend = yr_2024, color = direction),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = 1.5) +
  # Add rate values at arrow ends - adjusted positioning
  geom_text(data = filter(to_plot, direction == "Increased"), 
            aes(x = yr_2023, label = comma(yr_2023, 1)),
            hjust = 1, size = 3, nudge_x = -15, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Decreased"), 
            aes(x = yr_2023, label = comma(yr_2023, 1)),
            hjust = 1, size = 3, nudge_x = 55, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Increased"), 
            aes(x = yr_2024, label = comma(yr_2024, 1)),
            hjust = 1, size = 3, nudge_x = 55, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Decreased"), 
            aes(x = yr_2024, label = comma(yr_2024, 1)),
            hjust = 1, size = 3, nudge_x = -15, color = "gray20", family = "Inter") +
  geom_text(
    aes(x = 1300, label = percent(change_pct, accuracy = 1, style_positive = "plus",
    style_negative = "minus")),
    hjust = 1, size = 3, color = "gray20", family = "Inter"
  ) +
  # Use blue for decreases, purple for increases to match your reference
  scale_color_manual(values = c("Decreased" = "#4095B1", "Increased" = "#8B5A9C")) +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.0)), labels = label_comma(),
  breaks = c(0, 250, 500, 750, 1000)) +
  labs(
    title = "Change in Violent Crime, 2023 to 2024",
    subtitle = "Violent crime reported to police per 100,000 residents",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: FBI Uniform Crime Reporting Program"
  ) +
  theme_minimal(base_family = "Inter") +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 9, face = "bold", family = "Inter"),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 10, family = "Inter"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", family = "Inter"),
    plot.subtitle = element_text(family = "Inter"),
    plot.caption = element_text(size = 8, family = "Inter"),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("violent_crime_rate_change_2023_2024.png", width = 8, height = 10, dpi = 300, bg = "white")

to_plot <- viol_crime_state |> 
  bind_rows(viol_crime_us) |>
  filter(year %in% c(2019, 2024), !state_name %in% c("Illinois", "Hawaii", "Florida")) |> 
  select(year, state_name, solve_rate) |> 
  pivot_wider(names_from = year, values_from = solve_rate, names_prefix = "yr_") |> 
  mutate(
    change = yr_2024 - yr_2019,
    change_pct = (yr_2024 - yr_2019) / yr_2019,
    direction = if_else(change > 0, "Increased", "Decreased"),
    change = round(yr_2024 * 100, 1) - round(yr_2019 * 100, 1)
  ) |>
  # Reorder states by 2019 rate for better visual organization
  mutate(
    state_name = fct_reorder(state_name, yr_2024),
    state_name = fct_relevel(state_name, "United States Total", after = 51)
    )

# arrow plot of violent crime rate changes 2019 to 2024 by state# Create arrow plot showing violent crime rate changes 2019 to 2024
to_plot |>
  ggplot(aes(y = state_name)) +
  # Add light blue background for United States row
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 48.5, ymax = 49.5, 
           fill = "lightblue", alpha = 0.3) +
  geom_hline(yintercept = seq(0.5, 49.5, 1), color = "gray90", linewidth = 0.5) +
  # Draw arrows from 2019 to 2024 values
  geom_segment(aes(x = yr_2019, xend = yr_2024, color = direction),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = 1.5) +
  # Add rate values at arrow ends - adjusted positioning
  geom_text(data = filter(to_plot, direction == "Increased"), 
            aes(x = yr_2019, label = percent(yr_2019, 1)),
            hjust = 1, size = 3, nudge_x = -.01, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Decreased"), 
            aes(x = yr_2019, label = percent(yr_2019, 1)),
            hjust = 1, size = 3, nudge_x = .04, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Increased"), 
            aes(x = yr_2024, label = percent(yr_2024, 1)),
            hjust = 1, size = 3, nudge_x = .04, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Decreased"), 
            aes(x = yr_2024, label = percent(yr_2024, 1)),
            hjust = 1, size = 3, nudge_x = -.01, color = "gray20", family = "Inter") +
  geom_text(
    aes(x = 0.9, label = number(change, accuracy = 1, style_positive = "plus",
    style_negative = "minus")),
    hjust = 1, size = 3, color = "gray20", family = "Inter"
  ) +
  # Use blue for decreases, purple for increases to match your reference
  scale_color_manual(values = c("Increased" = "#4095B1", "Decreased" = "#8B5A9C")) +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.0)), labels = label_comma(),
  ) +
  labs(
    title = "Change in Violent Crime Solve Rate, 2019 to 2024",
    subtitle = "Percentage of violent crime reported to police solved",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: FBI Uniform Crime Reporting Program"
  ) +
  theme_minimal(base_family = "Inter") +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 9, face = "bold", family = "Inter"),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 10, family = "Inter"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", family = "Inter"),
    plot.subtitle = element_text(family = "Inter"),
    plot.caption = element_text(size = 8, family = "Inter"),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("violent_crime_solve_rate_change_2019_2024.png", width = 8, height = 10, dpi = 300, bg = "white")

to_plot <- viol_crime_state |> 
  bind_rows(viol_crime_us) |>
  filter(year %in% c(2023, 2024), !state_name %in% c("Illinois", "Hawaii", "Florida")) |> 
  select(year, state_name, solve_rate) |> 
  pivot_wider(names_from = year, values_from = solve_rate, names_prefix = "yr_") |> 
  mutate(
    change = yr_2024 - yr_2023,
    change_pct = (yr_2024 - yr_2023) / yr_2023,
    direction = if_else(change > 0, "Increased", "Decreased"),
    change = round(yr_2024 * 100, 1) - round(yr_2023 * 100, 1)
  ) |>
  # Reorder states by 2023 rate for better visual organization
  mutate(
    state_name = fct_reorder(state_name, yr_2024),
    state_name = fct_relevel(state_name, "United States Total", after = 51)
    )

# arrow plot of violent crime rate changes 2023 to 2024 by state# Create arrow plot showing violent crime rate changes 2023 to 2024
to_plot |>
  ggplot(aes(y = state_name)) +
  # Add light blue background for United States row
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 48.5, ymax = 49.5, 
           fill = "lightblue", alpha = 0.3) +
  geom_hline(yintercept = seq(0.5, 49.5, 1), color = "gray90", linewidth = 0.5) +
  # Draw arrows from 2023 to 2024 values
  geom_segment(aes(x = yr_2023, xend = yr_2024, color = direction),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = 1.5) +
  # Add rate values at arrow ends - adjusted positioning
  geom_text(data = filter(to_plot, direction == "Increased"), 
            aes(x = yr_2023, label = percent(yr_2023, 1)),
            hjust = 1, size = 3, nudge_x = -.01, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Decreased"), 
            aes(x = yr_2023, label = percent(yr_2023, 1)),
            hjust = 1, size = 3, nudge_x = .04, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Increased"), 
            aes(x = yr_2024, label = percent(yr_2024, 1)),
            hjust = 1, size = 3, nudge_x = .04, color = "gray20", family = "Inter") +
  geom_text(data = filter(to_plot, direction == "Decreased"), 
            aes(x = yr_2024, label = percent(yr_2024, 1)),
            hjust = 1, size = 3, nudge_x = -.01, color = "gray20", family = "Inter") +
  geom_text(
    aes(x = 0.9, label = number(change, accuracy = 1, style_positive = "plus",
    style_negative = "minus")),
    hjust = 1, size = 3, color = "gray20", family = "Inter"
  ) +
  # Use blue for decreases, purple for increases to match your reference
  scale_color_manual(values = c("Increased" = "#4095B1", "Decreased" = "#8B5A9C")) +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.0)), labels = label_comma(),
  ) +
  labs(
    title = "Change in Violent Crime Solve Rate, 2023 to 2024",
    subtitle = "Percentage of violent crime reported to police solved",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: FBI Uniform Crime Reporting Program"
  ) +
  theme_minimal(base_family = "Inter") +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 9, face = "bold", family = "Inter"),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 10, family = "Inter"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", family = "Inter"),
    plot.subtitle = element_text(family = "Inter"),
    plot.caption = element_text(size = 8, family = "Inter"),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("violent_crime_solve_rate_change_2023_2024.png", width = 8, height = 10, dpi = 300, bg = "white")







# # Function to prepare data for arrow plot
# prepare_arrow_data <- function(data, entity_col, value_col, year_col, 
#                               start_year, end_year, 
#                               highlight_entity = NULL, 
#                               highlight_position = NULL) {
  
#   # Filter and pivot data
#   plot_data <- data |>
#     filter({{ year_col }} %in% c(start_year, end_year)) |>
#     select({{ year_col }}, {{ entity_col }}, {{ value_col }}) |>
#     pivot_wider(
#       names_from = {{ year_col }}, 
#       values_from = {{ value_col }}, 
#       names_prefix = "yr_"
#     ) |>
#     mutate(
#       change = .data[[paste0("yr_", end_year)]] - .data[[paste0("yr_", start_year)]],
#       change_pct = change / .data[[paste0("yr_", start_year)]],
#       direction = if_else(change > 0, "Increased", "Decreased")
#     )
  
#   # Reorder entities by end year value
#   plot_data <- plot_data |>
#     mutate({{ entity_col }} := fct_reorder({{ entity_col }}, .data[[paste0("yr_", end_year)]]))
  
#   # Handle highlight entity positioning if specified
#   if (!is.null(highlight_entity) && !is.null(highlight_position)) {
#     plot_data <- plot_data |>
#       mutate({{ entity_col }} := fct_relevel({{ entity_col }}, highlight_entity, after = highlight_position))
#   }
  
#   # Add column names for easy reference
#   attr(plot_data, "start_col") <- paste0("yr_", start_year)
#   attr(plot_data, "end_col") <- paste0("yr_", end_year)
  
#   return(plot_data)
# }

# # Function to prepare data for arrow plot
# prepare_arrow_data <- function(data, entity_col, value_col, year_col, 
#                               start_year, end_year, 
#                               highlight_entity = NULL, 
#                               highlight_position = NULL) {
  
#   # Filter and pivot data
#   plot_data <- data |>
#     filter({{ year_col }} %in% c(start_year, end_year)) |>
#     select({{ year_col }}, {{ entity_col }}, {{ value_col }}) |>
#     pivot_wider(
#       names_from = {{ year_col }}, 
#       values_from = {{ value_col }}, 
#       names_prefix = "yr_"
#     ) |>
#     mutate(
#       change = .data[[paste0("yr_", end_year)]] - .data[[paste0("yr_", start_year)]],
#       change_pct = change / .data[[paste0("yr_", start_year)]],
#       direction = if_else(change > 0, "Increased", "Decreased")
#     )
  
#   # Reorder entities by end year value
#   plot_data <- plot_data |>
#     mutate({{ entity_col }} := fct_reorder({{ entity_col }}, .data[[paste0("yr_", end_year)]]))
  
#   # Handle highlight entity positioning if specified
#   if (!is.null(highlight_entity) && !is.null(highlight_position)) {
#     plot_data <- plot_data |>
#       mutate({{ entity_col }} := fct_relevel({{ entity_col }}, highlight_entity, after = highlight_position))
#   }
  
#   # Add column names for easy reference
#   attr(plot_data, "start_col") <- paste0("yr_", start_year)
#   attr(plot_data, "end_col") <- paste0("yr_", end_year)
  
#   # Determine if values are proportions (0-1 range) for formatting
#   value_range <- range(c(plot_data[[paste0("yr_", start_year)]], 
#                         plot_data[[paste0("yr_", end_year)]]), na.rm = TRUE)
#   attr(plot_data, "is_proportion") <- value_range[2] <= 1 && value_range[1] >= 0
  
#   return(plot_data)
# }

# # Function to create arrow plot
# create_arrow_plot <- function(data, entity_col, 
#                              start_year, end_year,
#                              title = NULL, subtitle = NULL, caption = NULL,
#                              highlight_entity = NULL,
#                              colors = c("Decreased" = "#4095B1", "Increased" = "#8B5A9C"),
#                              x_breaks = NULL, x_max_label = NULL,
#                              nudge_factor = NULL) {
  
#   # Get column names from attributes
#   start_col <- attr(data, "start_col")
#   end_col <- attr(data, "end_col")
#   is_proportion <- attr(data, "is_proportion")
  
#   # Get entity column as string for calculations
#   entity_var <- ensym(entity_col)
#   n_entities <- nrow(data)
  
#   # Calculate highlight position if specified
#   highlight_pos <- NULL
#   if (!is.null(highlight_entity)) {
#     highlight_pos <- which(levels(data[[as_label(entity_var)]]) == highlight_entity)
#     if (length(highlight_pos) == 0) highlight_pos <- n_entities
#   }
  
#   # Auto-calculate nudging based on data range if not provided
#   if (is.null(nudge_factor)) {
#     data_range <- max(data[[end_col]], data[[start_col]], na.rm = TRUE) - 
#                   min(data[[end_col]], data[[start_col]], na.rm = TRUE)
#     nudge_factor <- data_range * 0.02  # 2% of range
#   }
  
#   # Choose appropriate labeling function
#   if (is_proportion) {
#     label_func <- function(x) percent(x, accuracy = 1)
#     if (is.null(x_breaks)) x_breaks <- seq(0, 1, 0.25)
#   } else {
#     label_func <- function(x) comma(x, accuracy = 1)
#     if (is.null(x_breaks)) {
#       max_val <- max(data[[end_col]], data[[start_col]], na.rm = TRUE)
#       x_breaks <- pretty(c(0, max_val), n = 5)
#     }
#   }
  
#   # Calculate x_max_label if not provided
#   if (is.null(x_max_label)) {
#     x_max_label <- max(data[[end_col]], data[[start_col]], na.rm = TRUE) * 1.15
#   }
  
#   # Create base plot
#   p <- data |>
#     ggplot(aes(y = {{ entity_col }})) +
#     geom_hline(yintercept = seq(0.5, n_entities + 0.5, 1), color = "gray90", linewidth = 0.5)
  
#   # Add highlight background if specified
#   if (!is.null(highlight_pos)) {
#     p <- p + 
#       annotate("rect", xmin = -Inf, xmax = Inf, 
#                ymin = highlight_pos - 0.5, ymax = highlight_pos + 0.5, 
#                fill = "lightblue", alpha = 0.3)
#   }
  
#   # Add arrows and value labels with smart positioning
#   p <- p +
#     geom_segment(aes(x = .data[[start_col]], xend = .data[[end_col]], color = direction),
#                  arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
#                  linewidth = 1.5) +
#     # Start values
#     geom_text(data = filter(data, direction == "Increased"), 
#               aes(x = .data[[start_col]], label = label_func(.data[[start_col]])),
#               hjust = 1, size = 3, nudge_x = -nudge_factor, color = "gray20", family = "Inter") +
#     geom_text(data = filter(data, direction == "Decreased"), 
#               aes(x = .data[[start_col]], label = label_func(.data[[start_col]])),
#               hjust = 0, size = 3, nudge_x = nudge_factor, color = "gray20", family = "Inter") +
#     # End values
#     geom_text(data = filter(data, direction == "Increased"), 
#               aes(x = .data[[end_col]], label = label_func(.data[[end_col]])),
#               hjust = 0, size = 3, nudge_x = nudge_factor, color = "gray20", family = "Inter") +
#     geom_text(data = filter(data, direction == "Decreased"), 
#               aes(x = .data[[end_col]], label = label_func(.data[[end_col]])),
#               hjust = 1, size = 3, nudge_x = -nudge_factor, color = "gray20", family = "Inter") +
#     # Percentage change labels
#     geom_text(
#       aes(x = x_max_label, label = percent(change_pct, accuracy = 1, 
#                                           style_positive = "plus", style_negative = "minus")),
#       hjust = 1, size = 3, color = "gray20", family = "Inter"
#     )
  
#   # Apply scales and theme
#   p <- p +
#     scale_color_manual(values = colors) +
#     scale_x_continuous(
#       expand = expansion(mult = c(0.08, 0.0)), 
#       labels = if (is_proportion) label_percent() else label_comma(),
#       breaks = x_breaks
#     ) +
#     labs(title = title, subtitle = subtitle, x = NULL, y = NULL, color = NULL, caption = caption) +
#     theme_minimal(base_family = "Inter") +
#     theme(
#       legend.position = "none",
#       axis.text.y = element_text(size = 9, face = "bold", family = "Inter"),
#       axis.text.x = element_blank(),
#       axis.title = element_text(size = 10, family = "Inter"),
#       panel.grid = element_blank(),
#       plot.title = element_text(size = 14, face = "bold", family = "Inter"),
#       plot.subtitle = element_text(family = "Inter"),
#       plot.caption = element_text(size = 8, family = "Inter"),
#       plot.margin = margin(10, 10, 10, 10)
#     )
  
#   return(p)
# }


# # Prepare the data
# plot_data <- prepare_arrow_data(
#   data = bind_rows(viol_crime_state, viol_crime_us),
#   entity_col = state_name,
#   value_col = incidents_per_100k,
#   year_col = year,
#   start_year = 2023,
#   end_year = 2024,
#   highlight_entity = "United States Total",
#   highlight_position = 51
# )

# # Create the plot
# create_arrow_plot(
#   data = plot_data,
#   entity_col = state_name,
#   start_year = 2019,
#   end_year = 2024,
#   title = "Change in Violent Crime, 2019 to 2024",
#   subtitle = "Violent crime reported to police per 100,000 residents",
#   caption = "Source: FBI Uniform Crime Reporting Program",
#   highlight_entity = "United States Total",
#   x_breaks = c(0, 250, 500, 750, 1000),
#   x_max_label = 1300
# )


# # Prepare the data
# plot_data <- prepare_arrow_data(
#   data = bind_rows(viol_crime_state, viol_crime_us),
#   entity_col = state_name,
#   value_col = incidents_per_100k,
#   year_col = year,
#   start_year = 2023,
#   end_year = 2024,
#   highlight_entity = "United States Total",
#   highlight_position = 51
# )

# # Create the plot
# create_arrow_plot(
#   data = plot_data,
#   entity_col = state_name,
#   start_year = 2023,
#   end_year = 2024,
#   title = "Change in Violent Crime, 2023 to 2024",
#   subtitle = "Violent crime reported to police per 100,000 residents",
#   caption = "Source: FBI Uniform Crime Reporting Program",
#   highlight_entity = "United States Total",
#   x_breaks = c(0, 250, 500, 750, 1000),
#   x_max_label = 1300
# )

# ggsave("violent_crime_rate_change_2023_2024.png", width = 8, height = 10, dpi = 300, bg = "white")


# # Prepare the data
# plot_data <- prepare_arrow_data(
#   data = bind_rows(viol_crime_state, viol_crime_us),
#   entity_col = state_name,
#   value_col = solve_rate,
#   year_col = year,
#   start_year = 2019,
#   end_year = 2024,
#   highlight_entity = "United States Total",
#   highlight_position = 51
# )

# # Create the plot
# create_arrow_plot(
#   data = plot_data,
#   entity_col = state_name,
#   start_year = 2019,
#   end_year = 2024,
#   title = "Change in Violent Crime, 2019 to 2024",
#   subtitle = "Violent crime reported to police per 100,000 residents",
#   caption = "Source: FBI Uniform Crime Reporting Program",
#   highlight_entity = "United States Total",
#   x_breaks = c(0, .25, .5, 0.75, 1),
#   x_max_label = 50
# )

# ggsave("violent_crime_rate_change_2019_2024.png", width = 8, height = 10, dpi = 300, bg = "white")

# # Prepare solve rate data
# plot_data <- 
#   bind_rows(viol_crime_state, viol_crime_us) |> 
#   filter(!state_name %in% c("Illinois", "Hawaii", "Florida")) |> 
#   prepare_arrow_data(
#   entity_col = state_name,
#   value_col = solve_rate,
#   year_col = year,
#   start_year = 2019,
#   end_year = 2024,
#   highlight_entity = "United States Total",
#   highlight_position = 51
# )

# # Create the plot - no need to specify x_breaks or x_max_label
# create_arrow_plot(
#   data = plot_data,
#   entity_col = state_name,
#   start_year = 2019,
#   end_year = 2024,
#   title = "Change in Violent Crime Solve Rate, 2019 to 2024",
#   subtitle = "Proportion of violent crimes cleared by police",
#   caption = "Source: FBI Uniform Crime Reporting Program",
#   highlight_entity = "United States Total"
# )

# ggsave("violent_crime_solve_rate_change_2023_2024.png", width = 8, height = 10, dpi = 300, bg = "white")
