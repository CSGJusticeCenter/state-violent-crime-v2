## Share of US violent crime solved, 1965-2025
##
## Reads the series built by solved-unsolved-since-1960.R

library(tidyverse)
library(scales)

chart_colors <- c(line = "#0A8A6A", ink = "#0B1F35", muted = "#5B6B7C", surface = "#EEF3F9")
chart_font <- "Public Sans"

trend <- read_csv("analysis/solved-unsolved-since-1960.csv", show_col_types = FALSE)

### first, lowest and last years get labeled points
labeled <- bind_rows(
  slice_min(trend, year, n = 1),
  slice_min(trend, pct_solved, n = 1),
  slice_max(trend, year, n = 1)
) |>
  mutate(label = percent(pct_solved, 0.1))

chart <- ggplot(trend, aes(year, pct_solved)) +
  geom_line(color = chart_colors[["line"]], linewidth = 1) +
  geom_point(data = labeled, size = 2.6, stroke = 1.2, shape = 21, color = chart_colors[["line"]], fill = chart_colors[["surface"]]) +
  geom_text(
    data = labeled,
    aes(label = label),
    vjust = c(-1, 1.9, -1),
    color = chart_colors[["ink"]], family = chart_font, fontface = "bold", size = 3.6
  ) +
  scale_x_continuous(breaks = seq(1965, 2025, 10), expand = expansion(add = 2)) +
  scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 0.7, 0.1), labels = percent, expand = expansion(mult = 0)) +
  labs(
    title = "Share of violent crime solved in the US, 1965–2025",
    x = NULL, y = NULL,
    caption = str_c(
      "Solved share is violent crimes cleared divided by violent crimes reported. Clearances can include crimes from earlier years.\n",
      "Sources: FBI Crime Data Explorer; FBI Uniform Crime Reports via Jacob Kaplan; ",
      "Crime Index (AH Datalytics) historical tables, 1965–1978"
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = chart_font, base_size = 12) +
  theme(
    plot.background = element_rect(fill = chart_colors[["surface"]], color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#D3DCE6", linewidth = 0.4),
    axis.line.x = element_line(color = chart_colors[["ink"]], linewidth = 0.8),
    axis.text.x = element_text(color = chart_colors[["ink"]], face = "bold", size = 11),
    axis.text.y = element_text(color = chart_colors[["muted"]], size = 11),
    plot.title = element_text(color = chart_colors[["ink"]], face = "bold", size = 18, margin = margin(b = 16)),
    plot.title.position = "plot",
    plot.caption = element_text(color = chart_colors[["muted"]], size = 8.5, hjust = 0, lineheight = 1.1, margin = margin(t = 14)),
    plot.caption.position = "plot",
    plot.margin = margin(20, 20, 16, 20)
  )

ggsave("analysis/pct-solved-since-1960.png", chart, width = 9, height = 6, dpi = 200, device = ragg::agg_png, bg = chart_colors[["surface"]])
