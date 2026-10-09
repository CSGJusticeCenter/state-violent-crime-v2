library(tidyverse)
library(csgjcr)

jr_ggplot_theme <- theme_minimal(base_family = "Public Sans") +
  theme(
    plot.title = element_text(
      family = "Public Sans ExtraBold",
      size = 11,
      color = "black",
      margin = margin(0, 0, 5, 0),
      lineheight = 1.1,
    ),
    plot.subtitle = element_text(
      family = "Public Sans",
      size = 11,
      color = "black",
      margin = margin(0, 0, 10, 0)
    ),
    plot.caption = element_text(
      family = "Public Sans",
      size = 7,
      color = "black",
      margin = margin(10, 0, 0, 0),
      hjust = 0
    ),
    plot.caption.position = "plot",
    axis.text = element_text(size = 9),
    axis.text.x = element_text(margin = margin(5, 0, 0, 0)),
    axis.ticks.x = element_line(linewidth = 0.5, color = "grey80"),
    axis.ticks.length.x = unit(5, "pt"),
    axis.title = element_text(color = "gray25"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 0, 0, 0),
    plot.title.position = "plot",
    legend.position = "top",
    legend.justification = c(0, 0),
    # legend.location = "plot",
    legend.spacing.x = unit(2.25, "pt"),
    legend.key.size = unit(7.25, "pt"),
    legend.margin = margin(-5, 0, 5, 0),
    legend.box.spacing = unit(10, "pt"),
    legend.title = element_text(family = "Public Sans", size = 9, color = "black"),
    legend.text = element_text(
      family = "Public Sans",
      size = 9,
      color = "black",
      margin = margin(0, 8, 0, 0)
    ),
    strip.text = element_text(
      size = 9,
      family = "Public Sans ExtraBold",
      margin = margin(0, 0, 5, 0),
      color = "black"
    ),
    panel.spacing = unit(3, "lines"),
  )

jr_pal <- c("#0089b2", "#b25e15", "#5d8122", "#6D6595", "#857100", "#B64543", "#757575")

scale_fill_csg <- function(levels = NULL, reverse = FALSE, ...) {

  if (!is.null(levels)) {
    jr_pal <- jr_pal[1:levels]
  }

  if (reverse) {
    jr_pal <- rev(jr_pal)
  }

  scale_fill_manual(values = jr_pal, ...)
}

scale_color_csg <- function(levels = NULL, reverse = FALSE, ...) {

  if (!is.null(levels)) {
    jr_pal <- jr_pal[1:levels]
  }

  if (reverse) {
    jr_pal <- rev(jr_pal)
  }

  scale_color_manual(values = jr_pal, ...)
}

font_hoist <- function(family, silent = FALSE) {
  font_specs <- systemfonts::system_fonts() |>
    dplyr::filter(family == {{ family }}) |>
    dplyr::mutate(family = paste(family, style)) |>
    dplyr::select(plain = path, name = family)

  purrr::pwalk(as.list(font_specs), systemfonts::register_font)

  if (!silent)  message(paste0("Hoisted ", nrow(font_specs), " variants:\n",
                               paste(font_specs$name, collapse = "\n")))
}

font_hoist("Public Sans", silent = TRUE)
theme_set(jr_ggplot_theme)

sp_data_to_plot_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")

# read in crime data
srs_us <- read_rds(file.path(sp_data_to_plot_path, "srs_us.rds")) |>
  filter(crime_cat == "Violent", year >= 2012) |>
  mutate(change_rate = pct_unsolved - pct_unsolved[year == min(year)])


srs_us |>
  filter(year %in% range(year)) |>
  select(year, group, pct_unsolved, change_rate)

srs_us |>
  ggplot(aes(year, pct_unsolved, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(
    data = filter(srs_us, year == max(year)),
    aes(label = paste0(group, " – ", scales::percent(pct_unsolved, 1))),
    nudge_x = 0.9,
    xlim = c(2022.5, 2023),
    family = "Public Sans",
    size = 11 / .pt
  ) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +
  scale_y_continuous(labels = scales::label_percent(),
                     breaks = c(0.1, 0.3, 0.5, 0.7, 0.9),
                     expand = expansion(mult = 0)
  ) +
  scale_color_csg() +
  expand_limits(y = c(0.1, 0.9)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Unsolved rate of violent crime by offense",
    subtitle = "United States",
    caption = "FBI Crime in the United States, Table 25",
    x = NULL,
    y = NULL
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(r = 110)
  )

ggsave(
  filename = "unsolved-by-offense-us.png",
  dpi = 300,
  device = ragg::agg_png,
  height = 4,
  width = 7,
  bg = "white"
)


srs_us |>
  ggplot(aes(year, pct_unsolved, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(
    data = filter(srs_us, year == max(year)),
    aes(label = group),
    nudge_x = 0.9,
    xlim = c(2022.5, 2023),
    family = "Public Sans",
    size = 11 / .pt
  ) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +
  scale_y_continuous(labels = scales::label_percent(),
                     breaks = c(0.1, 0.3, 0.5, 0.7, 0.9),
                     expand = expansion(mult = 0)
                     ) +
  scale_color_csg() +
  expand_limits(y = c(0.1, 0.9)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Unsolved rate of violent crime by offense",
    subtitle = "United States",
    caption = "FBI Crime in the United States, Table 25",
    x = NULL,
    y = NULL
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(r = 80)
  )

ggsave(
  filename = "unsolved-by-offense-us-no-numbers.png",
  dpi = 300,
  device = ragg::agg_png,
  height = 4,
  width = 7,
  bg = "white"
)
