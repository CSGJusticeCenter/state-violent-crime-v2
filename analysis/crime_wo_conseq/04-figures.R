## Reference ggplot versions of the handout's charts, pages 1-4
##
## The handout draws its charts as SVG. These PNGs plot the same data so each
## chart can be checked against the code. Axis limits match the handout's SVG
## axes, so the script stops if data fall outside them. Needs the Public Sans
## font installed. Reads the CSVs from 01-national-series.R and
## 02-state-2020-2025.R. Run from the project root.

library(tidyverse)
library(scales)
library(ggrepel)
source("analysis/crime_wo_conseq/helpers.R")

chart_colors <- c(
  ink = "#001E35", blue = "#005DA6", light = "#CCE9FF", bubble = "#A9C6E2",
  muted = "#4B5D6E", grid = "#D5E0EA", surface = "#EEF4F9"
)
chart_font <- "Public Sans"
chart_source <- "Source: CSG Justice Center analysis of FBI Uniform Crime Reporting (UCR) Program data."

if (!chart_font %in% systemfonts::system_fonts()$family) stop(chart_font, " is not installed")

### stop if values fall outside an axis's fixed limits
check_limits <- function(x, limits) {
  if (any(x < limits[1] | x > limits[2])) stop("Values fall outside the axis limits ", str_flatten_comma(limits))
}

national <- read_national()
states <- read_states()
recent <- recent_years(national)

short_count <- function(x) if_else(x >= 1e6, sprintf("%.2fM", x / 1e6), sprintf("%.0fK", x / 1e3))

theme_handout <- function() {
  theme_minimal(base_family = chart_font, base_size = 11) +
    theme(
      plot.background = element_rect(fill = chart_colors[["surface"]], color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = chart_colors[["grid"]], linewidth = 0.4),
      axis.text = element_text(color = chart_colors[["ink"]]),
      axis.title = element_text(color = chart_colors[["ink"]], face = "bold", size = 9),
      plot.title = element_text(color = chart_colors[["ink"]], face = "bold", size = 14),
      plot.subtitle = element_text(color = chart_colors[["ink"]], size = 10, margin = margin(b = 8)),
      plot.title.position = "plot",
      plot.caption = element_text(color = chart_colors[["muted"]], size = 8, hjust = 0, lineheight = 1.1, margin = margin(t = 10)),
      plot.caption.position = "plot",
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(color = chart_colors[["ink"]], size = 9),
      plot.margin = margin(14, 18, 10, 14)
    )
}

save_chart <- function(plot, file, width = 7, height = 4.5) {
  ggsave(file.path(handout_dir, file), plot, width = width, height = height, dpi = 200, bg = chart_colors[["surface"]])
}

### Page 1: reported, solved and unsolved, 2020-2025 ---------------------------

check_limits(c(recent$incidents_reported, recent$incidents_solved), c(0, 1.5e6))

yearly_labels <- recent |>
  select(year, Reported = incidents_reported, Solved = incidents_solved) |>
  pivot_longer(-year, names_to = "series", values_to = "n")

p1_yearly <- ggplot(recent, aes(year)) +
  geom_ribbon(aes(ymin = incidents_solved, ymax = incidents_reported), fill = chart_colors[["light"]]) +
  geom_line(data = yearly_labels, aes(y = n, color = series), linewidth = 1.2) +
  geom_point(data = yearly_labels, aes(y = n, color = series), size = 2.4) +
  geom_text(
    data = yearly_labels, aes(y = n, label = short_count(n), color = series, vjust = if_else(series == "Reported", -1, 2)),
    family = chart_font, fontface = "bold", size = 3
  ) +
  annotate(
    "text", x = 2025.25, y = mean(c(last(recent$incidents_reported), last(recent$incidents_solved))),
    label = "Unsolved\n(shaded)", hjust = 0, family = chart_font, size = 3, color = chart_colors[["muted"]]
  ) +
  scale_color_manual(values = c(Reported = chart_colors[["ink"]], Solved = chart_colors[["blue"]]), name = NULL) +
  scale_x_continuous(breaks = 2020:2025, expand = expansion(add = c(0.3, 0.9))) +
  scale_y_continuous(
    limits = c(0, 1.5e6), breaks = c(5e5, 1e6, 1.5e6),
    labels = label_number(scale_cut = cut_short_scale()), expand = expansion(0)
  ) +
  labs(
    title = "Violent Crime Reported, Solved, and Unsolved in the U.S., 2020–2025",
    x = NULL, y = NULL, caption = chart_source
  ) +
  coord_cartesian(clip = "off") +
  theme_handout()

save_chart(p1_yearly, "p1-reported-solved.png")

### Page 1: backlog added since 2020 -------------------------------------------

backlog <- recent |>
  transmute(year, backlog = cumsum(incidents_unsolved))
check_limits(backlog$backlog, c(0, 5e6))

p1_backlog <- ggplot(backlog, aes(year, backlog)) +
  geom_area(fill = chart_colors[["ink"]]) +
  annotate(
    "text", x = 2024.9, y = last(backlog$backlog), label = str_c("+", sprintf("%.1fM", last(backlog$backlog) / 1e6)),
    hjust = 1, vjust = -0.6, family = chart_font, fontface = "bold", size = 4.5, color = chart_colors[["ink"]]
  ) +
  scale_x_continuous(breaks = 2020:2025, expand = expansion(0)) +
  scale_y_continuous(
    limits = c(0, 5e6), breaks = seq(1e6, 5e6, 1e6),
    labels = \(x) str_c("+", label_number(scale_cut = cut_short_scale())(x)), expand = expansion(0)
  ) +
  labs(
    title = "Backlog of Unsolved Violent Crime, 2020–2025",
    subtitle = "Unsolved violent crimes added since 2020, on top of unsolved crimes from before 2020",
    x = NULL, y = NULL, caption = chart_source
  ) +
  theme_handout()

save_chart(p1_backlog, "p1-backlog.png")

### Page 2: share solved by state ----------------------------------------------

### tile grid from the handout map, row 1 at the top
grid <- tribble(
  ~state_abbr, ~col, ~row,
  "AK", 1, 1, "ME", 11, 1,
  "VT", 10, 2, "NH", 11, 2,
  "WA", 1, 3, "ID", 2, 3, "MT", 3, 3, "ND", 4, 3, "MN", 5, 3, "IL", 6, 3,
  "WI", 7, 3, "MI", 8, 3, "NY", 9, 3, "RI", 10, 3, "MA", 11, 3,
  "OR", 1, 4, "NV", 2, 4, "WY", 3, 4, "SD", 4, 4, "IA", 5, 4, "IN", 6, 4,
  "OH", 7, 4, "PA", 8, 4, "NJ", 9, 4, "CT", 10, 4,
  "CA", 1, 5, "UT", 2, 5, "CO", 3, 5, "NE", 4, 5, "MO", 5, 5, "KY", 6, 5,
  "WV", 7, 5, "VA", 8, 5, "MD", 9, 5, "DE", 10, 5,
  "AZ", 2, 6, "NM", 3, 6, "KS", 4, 6, "AR", 5, 6, "TN", 6, 6, "NC", 7, 6,
  "SC", 8, 6, "DC", 9, 6,
  "OK", 4, 7, "LA", 5, 7, "MS", 6, 7, "AL", 7, 7, "GA", 8, 7,
  "HI", 1, 8, "TX", 4, 8, "FL", 9, 8
)

### bins use the rounded share the map prints
map_bins <- c(
  `40% or less` = "#085444", `41–49%` = "#0F9A7C", `50–59%` = "#2DEBC3",
  `60% or more` = "#DDFCF5", `No reliable data` = "#FFFFFF"
)

tiles <- states |>
  inner_join(grid, by = join_by(state_abbr), relationship = "one-to-one") |>
  mutate(
    pct_shown = round(100 * pct_solved),
    bin = case_when(
      no_data ~ "No reliable data",
      pct_shown <= 40 ~ "40% or less",
      pct_shown < 50 ~ "41–49%",
      pct_shown < 60 ~ "50–59%",
      .default = "60% or more"
    ),
    bin = factor(bin, names(map_bins)),
    label = str_c(state_abbr, "\n", if_else(no_data, "---", solved_label(pct_solved, year_missing)))
  )
stopifnot(nrow(tiles) == 51)

### states, not counting DC, below half solved
below_half <- filter(tiles, !no_data, pct_solved < 0.5)
map_subtitle <- str_c(
  "Solve rates were below 50% in ", sum(below_half$state_abbr != "DC"), " states",
  if ("DC" %in% below_half$state_abbr) " and DC" else ""
)

p2_map <- ggplot(tiles, aes(col, -row)) +
  geom_tile(aes(fill = bin, linetype = no_data), width = 0.92, height = 0.92, color = chart_colors[["muted"]], linewidth = 0.3) +
  geom_text(
    aes(label = label, color = bin == "40% or less"),
    family = chart_font, fontface = "bold", size = 3, lineheight = 0.9
  ) +
  scale_fill_manual(values = map_bins, name = NULL, drop = FALSE) +
  scale_color_manual(values = c(`TRUE` = "#FFFFFF", `FALSE` = "#000000"), guide = "none") +
  scale_linetype_manual(values = c(`TRUE` = "22", `FALSE` = "blank"), guide = "none") +
  coord_equal(expand = FALSE, clip = "off") +
  labs(
    title = "Share of Violent Crimes Solved, 2020–2025",
    subtitle = map_subtitle,
    x = NULL, y = NULL,
    caption = str_c("* Missing 2021 data. Illinois has no reliable clearance data.\n", chart_source)
  ) +
  theme_handout() +
  theme(axis.text = element_blank(), panel.grid.major.y = element_blank())

save_chart(p2_map, "p2-solved-map.png", width = 7.5, height = 6.2)

### Page 3: solved and unsolved by five-year period, 1966-2025 -----------------

five_year <- periods(filter(national, year >= 1966), 5) |>
  mutate(label = fct_inorder(label))
check_limits(five_year$incidents_reported, c(0, 2e6))

stacked <- five_year |>
  select(label, Solved = incidents_solved, Unsolved = incidents_unsolved) |>
  pivot_longer(-label, names_to = "status", values_to = "n") |>
  mutate(status = factor(status, c("Unsolved", "Solved")))

p3_stacked <- ggplot(stacked, aes(label, n, fill = status)) +
  geom_col(width = 0.72) +
  geom_text(
    data = five_year, aes(label, incidents_solved / 2, label = percent(pct_solved, 1)), inherit.aes = FALSE,
    color = "#FFFFFF", family = chart_font, fontface = "bold", size = 3.2
  ) +
  scale_fill_manual(
    values = c(Solved = chart_colors[["blue"]], Unsolved = chart_colors[["light"]]),
    labels = c(Solved = "Solved (solve rate labeled)", Unsolved = "Unsolved"),
    breaks = c("Solved", "Unsolved"), name = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 2e6), breaks = c(5e5, 1e6, 1.5e6, 2e6),
    labels = label_number(scale_cut = cut_short_scale()), expand = expansion(0)
  ) +
  labs(
    title = "Violent Crime Solved and Unsolved in the U.S., 1966–2025",
    subtitle = "Average violent crimes reported per year, by 5-year period",
    x = NULL, y = NULL,
    caption = str_c(chart_source, "\nFigures for 1966–1978 compiled by Crime Index.")
  ) +
  theme_handout() +
  theme(axis.text.x = element_text(size = 8))

save_chart(p3_stacked, "p3-solved-unsolved-5yr.png", width = 7.5)

### Page 4: crime rate against share solved by state ---------------------------

us_crime_rate <- pooled_crime_rate(states)
us_solve_rate <- pooled_solve_rate(recent)

scatter_rows <- states |>
  filter(!no_data) |>
  mutate(label = str_c(state_abbr, if_else(year_missing, "*", "")))
check_limits(scatter_rows$reported_per_100k, c(0, 1000))
check_limits(scatter_rows$pct_solved, c(0.24, 0.8))

quadrant_labels <- tribble(
  ~x, ~y, ~hjust, ~vjust, ~label,
  0, 0.8, 0, 1, "Less crime, more solved",
  1000, 0.8, 1, 1, "More crime, more solved",
  0, 0.24, 0, 0, "Less crime, less solved",
  1000, 0.24, 1, 0, "More crime, less solved"
)

p4_scatter <- ggplot(scatter_rows, aes(reported_per_100k, pct_solved)) +
  geom_vline(xintercept = us_crime_rate, color = chart_colors[["ink"]], linetype = "22", alpha = 0.6) +
  geom_hline(yintercept = us_solve_rate, color = chart_colors[["ink"]], linetype = "22", alpha = 0.6) +
  annotate(
    "text", x = us_crime_rate + 8, y = 0.25, hjust = 0, vjust = 0, family = chart_font, size = 2.8,
    color = chart_colors[["ink"]], label = str_c("U.S. rate, ", comma(us_crime_rate, 1))
  ) +
  ### left of DC's point, just above the line
  annotate(
    "text", x = 930, y = us_solve_rate + 0.008, hjust = 1, vjust = 0, family = chart_font, size = 2.8,
    color = chart_colors[["ink"]], label = str_c("U.S. solve rate, ", percent(us_solve_rate, 1))
  ) +
  geom_text(
    data = quadrant_labels, aes(x, y, label = label, hjust = hjust, vjust = vjust),
    family = chart_font, fontface = "bold", size = 3.1, color = chart_colors[["blue"]]
  ) +
  geom_point(aes(size = unsolved), shape = 21, fill = chart_colors[["bubble"]], color = chart_colors[["blue"]], stroke = 0.4) +
  geom_text_repel(
    aes(label = label, point.size = sqrt(unsolved / max(unsolved)) * 10),
    family = chart_font, fontface = "bold", size = 2.6, color = chart_colors[["ink"]],
    box.padding = 0.15, point.padding = 0.05, min.segment.length = 0.3,
    segment.color = chart_colors[["blue"]], segment.size = 0.25, seed = 1, max.overlaps = Inf
  ) +
  scale_x_continuous(labels = comma, limits = c(0, 1000), breaks = seq(0, 1000, 250), expand = expansion(0)) +
  scale_y_continuous(labels = percent, limits = c(0.24, 0.8), breaks = seq(0.3, 0.7, 0.1), expand = expansion(0)) +
  scale_size_area(
    max_size = 10, breaks = c(2.5e4, 1e5, 3e5), labels = label_number(scale_cut = cut_short_scale()),
    name = "Circle size: unsolved violent crimes, 2020–2025"
  ) +
  labs(
    title = "Share of Violent Crimes Solved and Violent Crime Rate by State, 2020–2025",
    x = "Violent crimes reported per 100,000 residents per year",
    y = "Share of violent crimes solved",
    caption = str_c(
      "* Solve rate excludes 2021 because of unreliable clearance data. Illinois has no reliable clearance data.\n",
      chart_source
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_handout() +
  theme(panel.grid.major.x = element_line(color = chart_colors[["grid"]], linewidth = 0.4))

save_chart(p4_scatter, "p4-crime-rate-vs-solved.png", width = 7.5, height = 4.8)
