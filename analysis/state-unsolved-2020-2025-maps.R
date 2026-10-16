## Tile maps and charts of solved and unsolved violent crime by state, 2020-2025
##
## Reads analysis/state-unsolved-2020-2025.csv from state-unsolved-2020-2025-data.R

library(tidyverse)
library(scales)
library(ggrepel)

chart_colors <- c(
  ink = "#0B1F35", muted = "#5B6B7C", grid = "#D3DCE6", surface = "#EEF3F9",
  tile = "#FFFFFF", solved = "#4FA6C8", unsolved = "#00475D", empty = "#C9D4DE"
)
chart_font <- "Public Sans"

caption <- str_c(
  "Pools 2020–2025. * One year hidden for low reporting or missing clearances; the rate pools the other years.\n",
  "Illinois clearance data are not reliable. Source: FBI Crime Data Explorer state estimates"
)

### tile grid from the published map, row 1 at the top
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

states <- read_csv("analysis/state-unsolved-2020-2025.csv", show_col_types = FALSE) |>
  inner_join(grid, by = join_by(state_abbr), relationship = "one-to-one") |>
  mutate(
    x = col,
    y = -row,
    star = if_else(year_missing & !no_data, "*", ""),
    solved_label = if_else(no_data, "n/a", str_c(percent(pct_solved, 1), star)),
    unsolved_label = if_else(no_data, "n/a", str_c(percent(pct_unsolved, 1), star))
  )
stopifnot(nrow(states) == 51)

half <- 0.44

theme_tiles <- function() {
  theme_void(base_family = chart_font, base_size = 12) +
    theme(
      plot.background = element_rect(fill = chart_colors[["surface"]], color = NA),
      plot.title = element_text(color = chart_colors[["ink"]], face = "bold", size = 18, margin = margin(b = 6)),
      plot.subtitle = element_text(color = chart_colors[["ink"]], size = 11.5, margin = margin(b = 10), lineheight = 1.15),
      plot.title.position = "plot",
      plot.caption = element_text(color = chart_colors[["muted"]], size = 8.5, hjust = 0, lineheight = 1.1, margin = margin(t = 10)),
      plot.caption.position = "plot",
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(color = chart_colors[["ink"]], size = 10),
      legend.text = element_text(color = chart_colors[["muted"]], size = 9.5),
      plot.margin = margin(16, 20, 10, 20)
    )
}

tile_base <- function(df = states) {
  ggplot(df, aes(x, y)) +
    coord_equal(xlim = c(0.5, 11.5), ylim = c(-8.5, -0.5), expand = FALSE, clip = "off")
}

no_data_tile <- function(df = states) {
  geom_tile(
    data = filter(df, no_data), width = 2 * half, height = 2 * half,
    fill = chart_colors[["surface"]], color = chart_colors[["muted"]], linetype = "22", linewidth = 0.4
  )
}

save_chart <- function(plot, file, width = 9.5, height = 7.4) {
  ggsave(file.path("analysis", file), plot, width = width, height = height, dpi = 200, bg = chart_colors[["surface"]])
}

### 1. Tile color by share solved ----------------------------------------------

solve_breaks <- c(0.35, 0.40, 0.45, 0.50, 0.55)
solve_colors <- c("#00475d", "#007392", "#4fa6c8", "#9ed4ef", "#d4e9f8", "#f3f9fd")

tile_color <- tile_base() +
  geom_tile(
    data = filter(states, !no_data), aes(fill = pct_solved),
    width = 2 * half, height = 2 * half, color = chart_colors[["surface"]], linewidth = 0.6
  ) +
  no_data_tile() +
  geom_text(
    aes(y = y + 0.13, label = state_abbr, color = pct_solved < 0.45 & !no_data),
    family = chart_font, fontface = "bold", size = 4
  ) +
  geom_text(
    aes(y = y - 0.17, label = solved_label, color = pct_solved < 0.45 & !no_data),
    family = chart_font, size = 3.4
  ) +
  scale_fill_stepsn(
    colors = solve_colors, breaks = solve_breaks, limits = c(0.25, 0.75),
    labels = percent, name = "Share of violent crimes solved", guide = guide_colorsteps(
      barwidth = unit(16, "lines"), barheight = unit(0.7, "lines"), title.position = "top", even.steps = TRUE
    )
  ) +
  scale_color_manual(values = c(`TRUE` = "#FFFFFF", `FALSE` = chart_colors[["ink"]]), guide = "none") +
  labs(
    title = "Fewer than half of violent crimes were solved in 30 states and DC",
    subtitle = "Share of violent crimes reported from 2020 to 2025 that police solved",
    caption = caption
  ) +
  theme_tiles()

save_chart(tile_color, "state-map-1-solved-color.png")

### 2. Tile filled to the share solved ------------------------------------------

fill_rows <- filter(states, !no_data)

### label strip on top, glass below it
glass_top <- 0.2
glass_bottom <- -half
glass_rows <- mutate(
  fill_rows,
  ymin = y + glass_bottom,
  ymax = y + glass_top,
  level = ymin + (ymax - ymin) * pct_solved,
  mid = (ymin + ymax) / 2
)

tile_fill <- tile_base() +
  geom_rect(
    data = glass_rows, aes(xmin = x - half, xmax = x + half, ymin = ymin, ymax = ymax),
    fill = chart_colors[["tile"]]
  ) +
  geom_rect(
    data = glass_rows, aes(xmin = x - half, xmax = x + half, ymin = ymin, ymax = level),
    fill = chart_colors[["solved"]]
  ) +
  geom_segment(
    data = glass_rows, aes(x = x - half, xend = x + half, y = mid, yend = mid),
    color = chart_colors[["ink"]], linewidth = 0.35, linetype = "22"
  ) +
  geom_rect(
    data = glass_rows, aes(xmin = x - half, xmax = x + half, ymin = ymin, ymax = ymax),
    fill = NA, color = chart_colors[["ink"]], linewidth = 0.5
  ) +
  no_data_tile() +
  geom_text(
    aes(x = x - half, y = y + 0.33, label = state_abbr),
    hjust = 0, family = chart_font, fontface = "bold", size = 3.6, color = chart_colors[["ink"]]
  ) +
  geom_text(
    aes(x = x + half, y = y + 0.33, label = solved_label),
    hjust = 1, family = chart_font, size = 3.2, color = chart_colors[["ink"]]
  ) +
  annotate(
    "text", x = 2.6, y = -1.1, hjust = 0, vjust = 0.5, lineheight = 1.1,
    family = chart_font, size = 3.3, color = chart_colors[["ink"]],
    label = "Blue fills each glass to the share solved.\nThe dashed line marks half."
  ) +
  labs(
    title = "In 30 states and DC, the share solved stays below the halfway line",
    subtitle = "Share of violent crimes reported from 2020 to 2025 that police solved",
    caption = caption
  ) +
  theme_tiles()

save_chart(tile_fill, "state-map-2-solved-fill.png")

### 3. Bar height by unsolved crimes per 100,000 residents ----------------------

bar_max <- 0.58
bar_scale <- bar_max / max(states$unsolved_per_100k, na.rm = TRUE)

tile_bars <- tile_base() +
  geom_tile(
    data = fill_rows, width = 2 * half, height = 2 * half,
    fill = chart_colors[["tile"]], color = chart_colors[["grid"]], linewidth = 0.4
  ) +
  geom_rect(
    data = fill_rows,
    aes(xmin = x - 0.2, xmax = x + 0.2, ymin = y - half, ymax = y - half + unsolved_per_100k * bar_scale),
    fill = chart_colors[["unsolved"]]
  ) +
  no_data_tile() +
  geom_text(
    aes(x = x - half + 0.05, y = y + half - 0.06, label = state_abbr),
    hjust = 0, vjust = 1, family = chart_font, fontface = "bold", size = 3.3, color = chart_colors[["ink"]]
  ) +
  geom_text(
    data = fill_rows,
    aes(x = x + half - 0.05, y = y + half - 0.06, label = comma(unsolved_per_100k, 1)),
    hjust = 1, vjust = 1, family = chart_font, size = 2.9, color = chart_colors[["muted"]]
  ) +
  labs(
    title = "DC and New Mexico left the most violent crimes unsolved per resident",
    subtitle = "Unsolved violent crimes per 100,000 residents per year, 2020–2025",
    caption = caption
  ) +
  theme_tiles()

save_chart(tile_bars, "state-map-3-unsolved-per-100k.png")

### 4. Square area by number unsolved, colored by majority unsolved --------------

square_max <- 0.56
square_scale <- square_max / sqrt(max(states$unsolved_all_years, na.rm = TRUE))

square_rows <- fill_rows |>
  mutate(
    side = sqrt(unsolved_all_years) * square_scale,
    majority = if_else(pct_unsolved > 0.5, "More than half unsolved", "Half or fewer unsolved")
  )

tile_squares <- tile_base() +
  geom_tile(
    data = fill_rows, width = 2 * half, height = 2 * half,
    fill = chart_colors[["tile"]], color = chart_colors[["grid"]], linewidth = 0.4
  ) +
  geom_rect(
    data = square_rows,
    aes(xmin = x - side / 2, xmax = x + side / 2, ymin = y - half + 0.04, ymax = y - half + 0.04 + side, fill = majority)
  ) +
  no_data_tile() +
  geom_text(
    aes(x = x - half + 0.05, y = y + half - 0.06, label = state_abbr),
    hjust = 0, vjust = 1, family = chart_font, fontface = "bold", size = 3.3, color = chart_colors[["ink"]]
  ) +
  geom_text(
    data = square_rows,
    aes(x = x + half - 0.05, y = y + half - 0.06, label = label_number(scale_cut = cut_short_scale(), accuracy = 1)(unsolved_all_years)),
    hjust = 1, vjust = 1, family = chart_font, size = 2.9, color = chart_colors[["muted"]]
  ) +
  scale_fill_manual(
    values = c("More than half unsolved" = chart_colors[["unsolved"]], "Half or fewer unsolved" = "#9ED4EF"),
    name = NULL
  ) +
  labs(
    title = "California and Texas hold over a quarter of unsolved violent crimes",
    subtitle = "Square area shows unsolved violent crimes from 2020 to 2025",
    caption = caption
  ) +
  theme_tiles()

save_chart(tile_squares, "state-map-4-unsolved-count.png")

### 5. Ranked bars, solved and unsolved summing to 100% --------------------------

ranked <- fill_rows |>
  mutate(state_abbr = fct_reorder(str_c(state_abbr, star), pct_solved)) |>
  select(state_abbr, Solved = pct_solved, Unsolved = pct_unsolved) |>
  pivot_longer(-state_abbr, names_to = "status", values_to = "share") |>
  mutate(status = factor(status, c("Unsolved", "Solved")))

ranked_bars <- ggplot(ranked, aes(share, state_abbr, fill = status)) +
  geom_col(width = 0.78, color = chart_colors[["surface"]], linewidth = 0.3) +
  geom_vline(xintercept = 0.5, color = chart_colors[["ink"]], linewidth = 0.5, linetype = "22") +
  geom_text(
    data = filter(ranked, status == "Solved"),
    aes(x = 0.012, label = percent(share, 1)),
    hjust = 0, family = chart_font, size = 2.6, color = chart_colors[["ink"]]
  ) +
  scale_x_continuous(labels = percent, breaks = seq(0, 1, 0.25), expand = expansion(0)) +
  scale_fill_manual(values = c(Solved = "#9ED4EF", Unsolved = chart_colors[["unsolved"]]), name = NULL, breaks = c("Solved", "Unsolved")) +
  labs(
    title = "Vermont solved 74% of violent crimes. New Mexico solved 30%",
    subtitle = "Share of violent crimes reported from 2020 to 2025, ranked by share solved",
    x = NULL, y = NULL,
    caption = caption
  ) +
  theme_minimal(base_family = chart_font, base_size = 11) +
  theme(
    plot.background = element_rect(fill = chart_colors[["surface"]], color = NA),
    panel.grid = element_blank(),
    axis.text.y = element_text(color = chart_colors[["ink"]], size = 8, face = "bold"),
    axis.text.x = element_text(color = chart_colors[["muted"]]),
    plot.title = element_text(color = chart_colors[["ink"]], face = "bold", size = 15),
    plot.subtitle = element_text(color = chart_colors[["ink"]], size = 11, margin = margin(b = 6)),
    plot.title.position = "plot",
    plot.caption = element_text(color = chart_colors[["muted"]], size = 8, hjust = 0, lineheight = 1.1),
    plot.caption.position = "plot",
    legend.position = "top",
    legend.justification = "left",
    plot.margin = margin(16, 20, 10, 16)
  )

save_chart(ranked_bars, "state-map-5-ranked.png", width = 8, height = 10)

### 6. Crime rate against share unsolved -----------------------------------------

years <- 6

### reference lines at the US crime rate and the US share unsolved, which
### pools every state but Illinois like the site's US solve rate
us_crime_rate <- sum(states$reported_all) / years / sum(states$pop_2025) * 1e5
us_unsolved <- sum(fill_rows$unsolved) / sum(fill_rows$reported)

regions <- read_csv("data/csg-regions.csv", show_col_types = FALSE)

### states, not counting DC, where more than half of violent crimes went unsolved
majority_unsolved <- filter(fill_rows, pct_unsolved > 0.5)
n_majority_states <- sum(majority_unsolved$state_abbr != "DC")
dc_majority <- "DC" %in% majority_unsolved$state_abbr
scatter_title <- str_c(
  "In ", n_majority_states, " states", if (dc_majority) " and DC" else "",
  ", most violent crimes from 2020 to 2025 went unsolved"
)

scatter_rows <- fill_rows |>
  left_join(regions, by = join_by(state_name), relationship = "one-to-one") |>
  mutate(crime_per_100k = reported_all / years / pop_2025 * 1e5)
stopifnot(!anyNA(scatter_rows$csg_region))

region_colors <- c(Southern = "#eb6834", Western = "#eda100", Midwestern = "#1baf7a", Eastern = "#2a78d6")

quadrant_labels <- tribble(
  ~crime_per_100k, ~pct_unsolved, ~hjust, ~vjust, ~label,
  1000, 0.735, 1, 1, "More crime,\nmore unsolved",
  1000, 0.255, 1, 0, "More crime,\nmore solved",
  0, 0.735, 0, 1, "Less crime,\nmore unsolved",
  0, 0.255, 0, 0, "Less crime,\nmore solved"
)

### points is the bubble layer, so the chart can use one color or color by region
scatter_plot <- function(points, ...) {
  ggplot(scatter_rows, aes(crime_per_100k, pct_unsolved)) +
    geom_hline(yintercept = us_unsolved, color = chart_colors[["muted"]], linewidth = 0.4, linetype = "22") +
    geom_vline(xintercept = us_crime_rate, color = chart_colors[["muted"]], linewidth = 0.4, linetype = "22") +
    annotate(
      "text", x = us_crime_rate + 8, y = 0.255, hjust = 0, vjust = 0, family = chart_font, size = 2.9,
      color = chart_colors[["muted"]], label = str_c("U.S. rate, ", comma(us_crime_rate, 1))
    ) +
    annotate(
      "text", x = 1000, y = us_unsolved + 0.005, hjust = 1, vjust = 0, family = chart_font, size = 2.9,
      color = chart_colors[["muted"]], label = str_c("U.S. share unsolved, ", percent(us_unsolved, 1))
    ) +
    geom_text(
      data = quadrant_labels, aes(label = label, hjust = hjust, vjust = vjust),
      family = chart_font, size = 3.2, fontface = "italic", lineheight = 1, color = "#9AA8B6"
    ) +
    points +
    geom_text_repel(
      aes(label = state_abbr, point.size = sqrt(unsolved_all_years / max(unsolved_all_years)) * 12),
      family = chart_font, fontface = "bold", size = 2.8, color = chart_colors[["ink"]],
      box.padding = 0.2, point.padding = 0.05, min.segment.length = 0.3,
      segment.color = chart_colors[["muted"]], segment.size = 0.25, seed = 1, max.overlaps = Inf
    ) +
    list(...) +
    scale_x_continuous(labels = comma, limits = c(0, 1000), expand = expansion(c(0, 0.08))) +
    scale_y_continuous(labels = percent, limits = c(0.25, 0.735)) +
    scale_size_area(
      max_size = 12, breaks = c(1e5, 3e5, 6e5), labels = label_number(scale_cut = cut_short_scale()),
      name = "Unsolved crimes", guide = guide_legend(override.aes = list(fill = chart_colors[["muted"]]), order = 2)
    ) +
    labs(
      title = scatter_title,
      subtitle = "Violent crime rate and share of violent crimes unsolved by state. Bubbles are sized by unsolved crimes",
      x = "Violent crimes per 100,000 residents per year",
      y = "Share of violent crimes unsolved",
      caption = caption
    ) +
    theme_minimal(base_family = chart_font, base_size = 11) +
    theme(
      plot.background = element_rect(fill = chart_colors[["surface"]], color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E2E8EF", linewidth = 0.3),
      axis.title = element_text(color = chart_colors[["muted"]], size = 10),
      axis.text = element_text(color = chart_colors[["muted"]]),
      plot.title = element_text(color = chart_colors[["ink"]], face = "bold", size = 16),
      plot.subtitle = element_text(color = chart_colors[["ink"]], size = 11, margin = margin(b = 8)),
      plot.title.position = "plot",
      plot.caption = element_text(color = chart_colors[["muted"]], size = 8, hjust = 0, lineheight = 1.1),
      plot.caption.position = "plot",
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(color = chart_colors[["ink"]], size = 10),
      plot.margin = margin(16, 20, 10, 16)
    )
}

scatter <- scatter_plot(
  geom_point(
    aes(size = unsolved_all_years),
    shape = 21, fill = chart_colors[["solved"]], color = chart_colors[["surface"]], stroke = 0.8, alpha = 0.9
  )
)
save_chart(scatter, "state-map-6-rate-vs-unsolved.png", width = 9.5, height = 7.4)

scatter_region <- scatter_plot(
  geom_point(
    aes(size = unsolved_all_years, fill = csg_region),
    shape = 21, color = chart_colors[["surface"]], stroke = 0.8, alpha = 0.9
  ),
  scale_fill_manual(
    values = region_colors, breaks = names(region_colors), name = NULL,
    guide = guide_legend(override.aes = list(size = 5), order = 1)
  )
)
save_chart(scatter_region, "state-map-6b-rate-vs-unsolved-region.png", width = 9.5, height = 7.4)
