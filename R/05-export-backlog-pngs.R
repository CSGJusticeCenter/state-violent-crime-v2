## static PNGs of the backlog section's first chart (violent crime reported,
## solved and unsolved) for Arkansas and the U.S., 2015-2025, styled like the
## site's Highcharts version
## needs the GT America font installed; run from the project root

library(tidyverse)
library(scales)

source("R/page-data.R")

export_dir <- "exports"
first_year <- 2015
last_year <- 2025
chart_font <- "GT America"

if (!chart_font %in% systemfonts::system_fonts()$family) stop(chart_font, " is not installed")

srs <- read_srs_state()
solve_coverage <- read_solve_coverage()

### same counts as the site's charts; add_backlog() blanks solve rates the site
### hides, and state totals sum their offenses with backlog_totals()
arkansas <- violent_by_offense(srs, first_year, "Arkansas") |>
  add_backlog(solve_coverage, from = first_year) |>
  backlog_totals() |>
  filter(year <= last_year)
us <- us_solve_trend(srs, first_year) |>
  add_backlog(solve_coverage, from = first_year) |>
  filter(year <= last_year)

### a hidden year would leave a gap these charts don't draw
if (anyNA(arkansas$incidents_solved) || anyNA(us$incidents_solved)) {
  stop("A year from ", first_year, " to ", last_year, " has no solve rate", call. = FALSE)
}

### point labels in the site's compact format, e.g., 1.31M, 563K, 25.3K or 840
compact_label <- function(x) {
  case_when(
    abs(x) >= 1e6 ~ sprintf("%.2fM", x / 1e6),
    abs(x) >= 1e5 ~ sprintf("%.0fK", x / 1e3),
    abs(x) >= 1e3 ~ sprintf("%.1fK", x / 1e3),
    .default = sprintf("%.0f", x)
  )
}

### site colors from R/page-data.R; label sizes are in mm, the site's px * 0.265
navy <- site_colors[["navy"]]
blue <- site_colors[["blue"]]
caption_text <- "FBI UCR Program, state crime estimates and clearances"

## Arkansas --------------------------------------------------------------------

arkansas_label_year <- mean(c(first_year, last_year))
arkansas_label_mid <- arkansas |>
  filter(year == arkansas_label_year) |>
  mutate(mid = (incidents_reported + incidents_solved) / 2)

arkansas_chart <- ggplot(arkansas, aes(year)) +
  geom_ribbon(aes(ymin = incidents_solved, ymax = incidents_reported), fill = site_colors[["pale_blue"]]) +
  geom_text(data = arkansas_label_mid, aes(y = mid), label = "Unsolved",
            family = chart_font, fontface = "bold", size = 4, color = site_colors[["text"]]) +
  geom_line(aes(y = incidents_reported), color = navy, linewidth = 1.1) +
  geom_line(aes(y = incidents_solved), color = blue, linewidth = 1.1, linetype = "22") +
  geom_point(aes(y = incidents_reported), shape = 21, size = 2.6, stroke = 0.8, fill = navy, color = "white") +
  geom_point(aes(y = incidents_solved), shape = 23, size = 2.8, stroke = 0.8, fill = blue, color = "white") +
  geom_text(aes(y = incidents_reported, label = compact_label(incidents_reported)),
            vjust = -1.1, family = chart_font, size = 3.4, color = navy) +
  geom_text(aes(y = incidents_solved, label = compact_label(incidents_solved)),
            vjust = 2, family = chart_font, size = 3.4, color = blue) +
  annotate("text", x = last_year + 0.25, y = last(arkansas$incidents_reported), label = "Reported",
           hjust = 0, family = chart_font, fontface = "bold", size = 4, color = navy) +
  annotate("text", x = last_year + 0.25, y = last(arkansas$incidents_solved), label = "Solved",
           hjust = 0, family = chart_font, fontface = "bold", size = 4, color = blue) +
  scale_x_continuous(breaks = first_year:last_year, expand = expansion(add = 0.4)) +
  ### 20% headroom above the largest count, like the site's chart
  scale_y_continuous(limits = c(0, max(arkansas$incidents_reported) * 1.2),
                     labels = label_number(accuracy = 0.1, drop0trailing = TRUE, scale_cut = cut_short_scale()),
                     expand = expansion(0)) +
  labs(title = paste0("Violent crime reported, solved, and unsolved in Arkansas, ", first_year, "-", last_year),
       x = NULL, y = NULL, caption = caption_text) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = chart_font, base_size = 10) +
  theme(
    plot.background = element_rect(fill = "white", color = site_colors[["border"]], linewidth = 0.6),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = site_colors[["grid"]], linewidth = 0.4),
    axis.line.x = element_line(color = site_colors[["baseline"]], linewidth = 0.4),
    axis.ticks.x = element_line(color = site_colors[["baseline"]], linewidth = 0.4),
    axis.text = element_text(color = site_colors[["muted"]], size = 10),
    plot.title = element_text(color = site_colors[["text"]], face = "bold", size = 16.5, margin = margin(b = 12)),
    plot.title.position = "plot",
    plot.caption = element_text(color = site_colors[["muted"]], size = 10, hjust = 1, margin = margin(t = 12)),
    plot.caption.position = "plot",
    ### room on the right for the line names
    plot.margin = margin(14, 70, 10, 14)
  )

arkansas_chart

ggsave(file.path(export_dir, "backlog-trend-arkansas-2015-2025.png"), arkansas_chart,
       width = 9, height = 5.2, dpi = 300, bg = "white", device = ragg::agg_png)

## United States ---------------------------------------------------------------

us_label_year <- mean(c(first_year, last_year))
us_label_mid <- us |>
  filter(year == us_label_year) |>
  mutate(mid = (incidents_reported + incidents_solved) / 2)

us_chart <- ggplot(us, aes(year)) +
  geom_ribbon(aes(ymin = incidents_solved, ymax = incidents_reported), fill = site_colors[["pale_blue"]]) +
  geom_text(data = us_label_mid, aes(y = mid), label = "Unsolved",
            family = chart_font, fontface = "bold", size = 4, color = site_colors[["text"]]) +
  geom_line(aes(y = incidents_reported), color = navy, linewidth = 1.1) +
  geom_line(aes(y = incidents_solved), color = blue, linewidth = 1.1, linetype = "22") +
  geom_point(aes(y = incidents_reported), shape = 21, size = 2.6, stroke = 0.8, fill = navy, color = "white") +
  geom_point(aes(y = incidents_solved), shape = 23, size = 2.8, stroke = 0.8, fill = blue, color = "white") +
  geom_text(aes(y = incidents_reported, label = compact_label(incidents_reported)),
            vjust = -1.1, family = chart_font, size = 3.4, color = navy) +
  geom_text(aes(y = incidents_solved, label = compact_label(incidents_solved)),
            vjust = 2, family = chart_font, size = 3.4, color = blue) +
  annotate("text", x = last_year + 0.25, y = last(us$incidents_reported), label = "Reported",
           hjust = 0, family = chart_font, fontface = "bold", size = 4, color = navy) +
  annotate("text", x = last_year + 0.25, y = last(us$incidents_solved), label = "Solved",
           hjust = 0, family = chart_font, fontface = "bold", size = 4, color = blue) +
  scale_x_continuous(breaks = first_year:last_year, expand = expansion(add = 0.4)) +
  ### 20% headroom above the largest count, like the site's chart
  scale_y_continuous(limits = c(0, max(us$incidents_reported) * 1.2),
                     labels = label_number(accuracy = 0.1, drop0trailing = TRUE, scale_cut = cut_short_scale()),
                     expand = expansion(0)) +
  labs(title = paste0("Violent crime reported, solved, and unsolved in the U.S., ", first_year, "-", last_year),
       x = NULL, y = NULL, caption = caption_text) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = chart_font, base_size = 10) +
  theme(
    plot.background = element_rect(fill = "white", color = site_colors[["border"]], linewidth = 0.6),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = site_colors[["grid"]], linewidth = 0.4),
    axis.line.x = element_line(color = site_colors[["baseline"]], linewidth = 0.4),
    axis.ticks.x = element_line(color = site_colors[["baseline"]], linewidth = 0.4),
    axis.text = element_text(color = site_colors[["muted"]], size = 10),
    plot.title = element_text(color = site_colors[["text"]], face = "bold", size = 16.5, margin = margin(b = 12)),
    plot.title.position = "plot",
    plot.caption = element_text(color = site_colors[["muted"]], size = 10, hjust = 1, margin = margin(t = 12)),
    plot.caption.position = "plot",
    ### room on the right for the line names
    plot.margin = margin(14, 70, 10, 14)
  )

us_chart

ggsave(file.path(export_dir, "backlog-trend-us-2015-2025.png"), us_chart,
       width = 9, height = 5.2, dpi = 300, bg = "white", device = ragg::agg_png)
