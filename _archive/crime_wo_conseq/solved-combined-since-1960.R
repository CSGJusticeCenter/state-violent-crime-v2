## Solved and unsolved violent crime with solve rates, 1966-2025
##
## Four ways to show counts and the solve rate without a second y-axis.
## Reads the series built by solved-unsolved-since-1960.R

library(tidyverse)
library(scales)
library(patchwork)

chart_colors <- c(
  reported = "#0B1F35", solved = "#0A8A6A", gap = "#B9CBD8", bar = "#0A8A6A",
  ink = "#0B1F35", muted = "#5B6B7C", grid = "#D3DCE6", surface = "#EEF3F9"
)
chart_font <- "Public Sans"

last_year <- 2025

chart_caption <- function(first_year) {
  str_c(
    "Solved counts apply clearance rates to estimated violent crimes. Clearances can include crimes from earlier years.\n",
    "Sources: FBI Crime Data Explorer; FBI Uniform Crime Reports via Jacob Kaplan; ",
    "Crime Index (AH Datalytics) historical tables, ", first_year, "–1978"
  )
}

short_count <- function(x) if_else(x >= 1e6, sprintf("%.2fM", x / 1e6), sprintf("%.0fK", x / 1e3))

trend_all <- read_csv("analysis/solved-unsolved-since-1960.csv", show_col_types = FALSE) |>
  select(year, reported = incidents_reported, solved = incidents_solved, pct_solved)

trend_from <- function(first_year) filter(trend_all, year >= first_year)

### shared x range keeps stacked panels aligned; the left pad holds first-year labels
x_scale <- function(first_year) {
  scale_x_continuous(
    breaks = seq(1970, 2020, 10),
    limits = c(first_year - 6, last_year + 2.5),
    expand = expansion(0)
  )
}

### periods of `width` years from the first year, with the rate pooled as
### total solved over total reported
periods <- function(df, width) {
  first_year <- min(df$year)

  df |>
    mutate(start = first_year + (year - first_year) %/% width * width) |>
    summarize(end = max(year), years = n(), across(c(reported, solved), sum), .by = start) |>
    mutate(
      pct_solved = solved / reported,
      label = str_c(start, "–", if_else(end %/% 100 == start %/% 100, str_sub(end, 3), as.character(end))),
      mid = (start + end) / 2,
      across(c(reported, solved), \(x) x / years)
    )
}

base_theme <- function() {
  theme_minimal(base_family = chart_font, base_size = 12) +
    theme(
      plot.background = element_rect(fill = chart_colors[["surface"]], color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = chart_colors[["grid"]], linewidth = 0.4),
      axis.line.x = element_line(color = chart_colors[["ink"]], linewidth = 0.8),
      axis.text.x = element_text(color = chart_colors[["ink"]], face = "bold", size = 11),
      axis.text.y = element_text(color = chart_colors[["muted"]], size = 11),
      plot.title = element_text(color = chart_colors[["ink"]], face = "bold", size = 18, margin = margin(b = 12)),
      plot.subtitle = element_text(color = chart_colors[["ink"]], face = "bold", size = 12, margin = margin(b = 8)),
      plot.title.position = "plot",
      plot.caption = element_text(color = chart_colors[["muted"]], size = 8.5, hjust = 0, lineheight = 1.1, margin = margin(t = 14)),
      plot.caption.position = "plot",
      plot.margin = margin(12, 90, 8, 20)
    )
}

### reported and solved lines with the unsolved gap shaded. First-year values
### sit left of their points, peak-year values above reported and below solved,
### last-year values beside the series names
counts_panel <- function(df) {
  first <- slice_min(df, year, n = 1)
  peak <- slice_max(df, reported, n = 1)
  last <- slice_max(df, year, n = 1)

  value_labels <- tibble(
    year = c(first$year, first$year, peak$year, peak$year),
    value = c(first$reported, first$solved, peak$reported, peak$solved),
    series = c("reported", "solved", "reported", "solved"),
    x = c(first$year - 1.2, first$year - 1.2, peak$year, peak$year),
    hjust = c(1, 1, 0.5, 0.5),
    vjust = c(0.5, 0.5, -0.9, 1.9)
  ) |>
    mutate(label = short_count(value))

  points <- bind_rows(
    select(value_labels, year, value, series),
    tibble(year = last$year, value = c(last$reported, last$solved), series = c("reported", "solved"))
  )

  end_labels <- tibble(
    y = c(last$reported, (last$reported + last$solved) / 2, last$solved),
    label = c(
      str_c("Reported\n", short_count(last$reported)),
      str_c("Unsolved\n", short_count(last$reported - last$solved)),
      str_c("Solved\n", short_count(last$solved))
    ),
    color = chart_colors[c("reported", "muted", "solved")]
  )

  ggplot(df, aes(year)) +
    geom_ribbon(aes(ymin = solved, ymax = reported), fill = chart_colors[["gap"]]) +
    geom_line(aes(y = reported), color = chart_colors[["reported"]], linewidth = 1) +
    geom_line(aes(y = solved), color = chart_colors[["solved"]], linewidth = 1) +
    geom_point(
      data = points, aes(y = value, color = series),
      size = 2.4, stroke = 1.1, shape = 21, fill = chart_colors[["surface"]]
    ) +
    geom_text(
      data = value_labels,
      aes(x = x, y = value, label = label, color = series, hjust = hjust, vjust = vjust),
      family = chart_font, fontface = "bold", size = 3.4
    ) +
    annotate(
      "text", x = last$year + 1.5, y = end_labels$y, label = end_labels$label, color = end_labels$color,
      hjust = 0, lineheight = 0.9, family = chart_font, fontface = "bold", size = 3.8
    ) +
    scale_color_manual(values = chart_colors[c("reported", "solved")], guide = "none") +
    x_scale(first$year) +
    scale_y_continuous(
      limits = c(0, 2.2e6), breaks = seq(0, 2e6, 5e5),
      labels = label_number(scale_cut = cut_short_scale(), drop0trailing = TRUE),
      expand = expansion(0)
    ) +
    labs(x = NULL, y = NULL, subtitle = "Violent crimes reported and solved") +
    coord_cartesian(clip = "off") +
    base_theme()
}

rate_line_panel <- function(df) {
  labeled <- bind_rows(slice_min(df, year, n = 1), slice_min(df, pct_solved, n = 1), slice_max(df, year, n = 1))

  ggplot(df, aes(year, pct_solved)) +
    geom_line(color = chart_colors[["solved"]], linewidth = 1) +
    geom_point(data = labeled, size = 2.4, stroke = 1.1, shape = 21, color = chart_colors[["solved"]], fill = chart_colors[["surface"]]) +
    geom_text(
      data = labeled, aes(label = percent(pct_solved, 1)), vjust = c(-0.9, 1.8, -0.9),
      color = chart_colors[["ink"]], family = chart_font, fontface = "bold", size = 3.5
    ) +
    x_scale(min(df$year)) +
    scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 0.6, 0.2), labels = percent, expand = expansion(0)) +
    labs(x = NULL, y = NULL, subtitle = "Solve rate") +
    coord_cartesian(clip = "off") +
    base_theme()
}

### bars span their years; decades are labeled inside the bars
rate_bar_panel <- function(df, width, subtitle) {
  bars <- periods(df, width)

  chart <- ggplot(bars, aes(mid, pct_solved)) +
    geom_col(aes(width = end - start + 0.6), fill = chart_colors[["bar"]]) +
    geom_text(
      aes(label = percent(pct_solved, 1)), vjust = -0.5,
      color = chart_colors[["ink"]], family = chart_font, fontface = "bold", size = if (width < 10) 3.1 else 3.6
    )

  if (width == 10) {
    chart <- chart +
      geom_text(
        aes(y = 0.04, label = str_c(start, "s")), vjust = 0,
        color = "white", family = chart_font, fontface = "bold", size = 3.4
      )
  }

  chart +
    x_scale(min(df$year)) +
    scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 0.6, 0.2), labels = percent, expand = expansion(0)) +
    labs(x = NULL, y = NULL, subtitle = subtitle) +
    coord_cartesian(clip = "off") +
    base_theme()
}

### the bottom panel carries the x axis labels
stack_panels <- function(top, bottom, title, first_year, heights = c(2.2, 1)) {
  top <- top + theme(axis.text.x = element_blank())

  (top / bottom) +
    plot_layout(heights = heights) +
    plot_annotation(title = title, caption = chart_caption(first_year), theme = base_theme())
}

save_chart <- function(chart, file, height = 7.5) {
  ggsave(file, chart, width = 9, height = height, dpi = 200, device = ragg::agg_png, bg = chart_colors[["surface"]])
}

since_1966 <- trend_from(1966)
since_1970 <- trend_from(1970)

### 1. Counts over annual solve-rate line -------------------------------------

stack_panels(counts_panel(since_1966), rate_line_panel(since_1966), "Violent crime reported and solved in the US, 1966–2025", 1966) |>
  save_chart("analysis/combined-1-rate-line.png")

### 2. Counts over 5-year solve-rate bars --------------------------------------

stack_panels(
  counts_panel(since_1966),
  rate_bar_panel(since_1966, 5, "Solve rate by 5-year periods"),
  "Violent crime reported and solved in the US, 1966–2025",
  1966
) |>
  save_chart("analysis/combined-2-rate-bars-5yr.png")

### 3. Counts over decade solve-rate bars --------------------------------------

stack_panels(
  counts_panel(since_1970),
  rate_bar_panel(since_1970, 10, "Solve rate by decade"),
  "Violent crime reported and solved in the US, 1970–2025",
  1970
) |>
  save_chart("analysis/combined-3-rate-bars-10yr.png")

### 4. One chart: stacked solved and unsolved columns, solve rate labeled -----

five_year <- periods(since_1966, 5) |>
  mutate(unsolved = reported - solved)

stacked <- five_year |>
  select(label, solved, unsolved) |>
  pivot_longer(-label, names_to = "status", values_to = "n") |>
  mutate(status = factor(status, c("unsolved", "solved")), label = fct_inorder(label))

stacked_chart <- ggplot(stacked, aes(label, n, fill = status)) +
  geom_col(width = 0.75, color = chart_colors[["surface"]], linewidth = 0.5) +
  geom_text(
    data = five_year, aes(label, solved / 2, label = percent(pct_solved, 1)), inherit.aes = FALSE,
    color = "white", family = chart_font, fontface = "bold", size = 3.6
  ) +
  scale_fill_manual(
    values = c(solved = chart_colors[["solved"]], unsolved = chart_colors[["gap"]]),
    labels = c(solved = "Solved (solve rate labeled)", unsolved = "Unsolved"),
    breaks = c("solved", "unsolved"), name = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 2e6), breaks = seq(0, 2e6, 5e5),
    labels = label_number(scale_cut = cut_short_scale(), drop0trailing = TRUE),
    expand = expansion(0)
  ) +
  labs(
    title = "Violent crime solved and unsolved in the US, 1966–2025",
    subtitle = "Average violent crimes per year, by 5-year period",
    x = NULL, y = NULL, caption = chart_caption(1966)
  ) +
  base_theme() +
  theme(
    axis.text.x = element_text(size = 8.5),
    legend.position = "top", legend.justification = "left", legend.text = element_text(color = chart_colors[["ink"]], size = 11),
    plot.margin = margin(20, 20, 16, 20)
  )

save_chart(stacked_chart, "analysis/combined-4-stacked-5yr.png", height = 6)
