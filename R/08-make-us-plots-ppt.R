## code for plots for marshall's ppt

library(tidyverse)
library(scales)
library(ragg)
library(csgjcr)
library(ggrepel)

# path to data folder on sharepoint
sp_path <- csg_sp_path

viol_crime_by_off_state <- read_rds(file.path(sp_path, "viol_crime_by_off_state.rds"))
viol_crime_by_state <- read_rds(file.path(sp_path, "viol_crime_by_state.rds"))
viol_crime_by_off_us <- read_rds(file.path(sp_path, "viol_crime_by_off_us.rds"))
viol_crime_us <- read_rds(file.path(sp_path, "viol_crime_us.rds"))
us_prison <- read_rds(file.path(sp_path, "us_prison.rds"))
viol_prison_state <- read_rds(file.path(sp_path, "viol_prison_state.rds"))

t <- theme_minimal(base_family = "Tahoma") +
  theme(
    plot.title = element_text(
      family = "Tahoma",
      face = "bold",
      size = 18,
      color = "gray25",
      margin = margin(0, 0, 15, 0)
    ),
    plot.subtitle = element_text(
      family = "Tahoma",
      size = 15,
      color = "gray25",
      margin = margin(-10, 0, 15, 0)
    ),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(margin = margin(b = 10)),
    axis.text.y = element_text(margin = margin(l = 10)),
    axis.title = element_text(color = "gray25"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "top",
    legend.justification = c(0, 0),
    legend.text = element_text(family = "Tahoma", size = 12, color = "gray25"),
    strip.text = element_text(
      family = "Tahoma",
      face = "bold",
      size = 15,
      color = "gray25",
      margin = margin(10, 0, 15, 0)
    ),
    panel.spacing = unit(2.5, "lines")
  )

theme_set(t)

jr_pal <- c("#4095B1", "#273C4C", "#50A25D", "#E17619", "#E25449", "#779F38", "#AFABAB")

scale_color_csg <- function(levels = NULL, reverse = FALSE) {

  if (!is.null(levels)) {
    jr_pal <- jr_pal[1:levels]
  }

  if (reverse) {
    jr_pal <- rev(jr_pal)
  }

  scale_color_manual(values = jr_pal)
}

scale_fill_csg <- function(levels = NULL, reverse = FALSE) {

  if (!is.null(levels)) {
    jr_pal <- jr_pal[1:levels]
  }

  if (reverse) {
    jr_pal <- rev(jr_pal)
  }

  scale_fill_manual(values = jr_pal)
}

viol_crime_by_off_us |>
  arrange(year) |>
  filter(year %in% c(2020, 2010)) |>
  group_by(crime) |>
  mutate(
    rate = actual / pop_total * 1e5,
    pct_change = (rate - lag(rate)) / lag(rate)
    )

viol_crime_by_off_us |>
  arrange(year) |>
  filter(year %in% c(2020, 2019)) |>
  group_by(crime) |>
  mutate(
    rate = actual / pop_total * 1e5,
    pct_change = (rate - lag(rate)) / lag(rate)
    )


to_plot <- viol_crime_by_off_us |>
  mutate(
    rate = actual / pop_total * 1e5,
    crime = fct_reorder(crime, actual)
    )

to_plot |>
  ggplot(aes(year, rate, fill = crime)) +
  geom_col(alpha = 0.9, width = 0.8) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  scale_y_continuous(
    # breaks = seq(0, 1250000, by = 250000),
    # labels = label_number(scale_cut = cut_short_scale()),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_fill_csg() +
  labs(
    title = "Violent crime reported to police per 100k, by offense",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    legend.justification = c(0.5, 0.5)
    )

ggsave(
  "png/total-violent-offenses.png",
  device = ragg::agg_png,
  dpi = 300,
  height = 6,
  width = 10,
  units = "in",
  bg = "white"
)

to_plot <- viol_crime_by_off_us |>
  mutate(crime = fct_reorder(crime, clearance_rate))

to_plot |>
  ggplot(aes(year, clearance_rate, color = crime)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  geom_text_repel(
    data = filter(to_plot, year == max(year)),
    aes(label = crime),
    hjust = 0,
    nudge_x = 0.15,
    size = 5,
    family = "Tahoma",
    xlim = 2020.2
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(labels = label_percent(1)) +
  scale_color_manual(values = jr_pal[c(3, 2, 4, 1)]) +
  expand_limits(y = c(0.1, 0.7)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Clearance rate of violent crime reported to police, by offense",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 120, 20, 20)
  )

ggsave(
  "png/clearance-rate-offenses.png",
  device = ragg::agg_png,
  dpi = 300,
  height = 6,
  width = 12,
  units = "in",
  bg = "white"
)

to_plot <- us_prison |>
  mutate(
    non_violent = total - violent,
    pct_violent = violent / total
  ) |>
  pivot_longer(-c(year, pct_violent, total)) |>
  mutate(name = if_else(name == "violent", "Violent", "Nonviolent"))

to_plot |>
  ggplot(aes(year, value, fill = name)) +
  geom_col(alpha = 0.9, width = 0.8) +
  geom_text(
    data = filter(to_plot, name == "Violent"),
    aes(label = percent(pct_violent, 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    family = "Tahoma",
    fontface = "bold"
  ) +
  # geom_text(
  #   data = filter(to_plot, name == "Violent"),
  #   aes(
  #     label = number(total, 0.01, scale_cut = cut_short_scale()),
  #     y = total
  #     ),
  #   vjust = -0.2,
  #   color = "gray30",
  #   family = "Franklin Gothic Demi"
  # ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  scale_y_continuous(
    breaks = seq(0, 1250000, by = 250000),
    labels = label_number(scale_cut = cut_short_scale()),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_fill_manual(values = jr_pal[c(7, 6)]) +
  labs(
    title = "People sentenced in state prison",
    # subtitle = str_wrap("The total prison population has fallen by 10% since 2010, while the share of people in prison for violent offenses has increased from 53% to 58%.", 75),
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    legend.justification = c(0.5, 0.5)
  )

ggsave(
  "png/violent-prison-pop.png",
  device = ragg::agg_png,
  dpi = 300,
  height = 6,
  width = 12,
  units = "in",
  bg = "white"
)


to_plot <- viol_crime_by_state |>
  filter(year == 2019, state != "Illinois") |>
  mutate(crime_rate = actual / pop_total)


to_plot |>
  select(clearance_rate, crime_rate) |>
  cov.wt(wt = to_plot$pop_total, cor = TRUE)

a <- cor(to_plot$clearance_rate, to_plot$crime_rate)

a^2

cor.test(to_plot$clearance_rate, to_plot$crime_rate)

to_plot |>
  ggplot(aes(clearance_rate, crime_rate)) +
  geom_point(aes(size = pop_total), alpha = 0.5, color = jr_pal[1]) +
  geom_text_repel(
    data = filter(to_plot, state_abb %in% c("DC", "AK", "VT", "ID", "CA", "NY", "NM",
                                            "OH", "TX", "FL", "HI", "MS", "GA", "DE")),
    aes(label = state_abb),
    family = "Tahoma",
    fontface = "bold",
    size = 3.5
  ) +
  # geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = label_percent(1)) +
  scale_y_continuous(labels = label_number(scale = 1e5, big.mark = ",")) +
  scale_size(range = c(2, 10)) +
  labs(
    title = "Rates of reported violent crime and clearances by state, 2019",
    subtitle = "Size of circles are proportionate to state population",
    x = "Violent crime clearance rate",
    y = "Reported violent crime rate (per 100k)",
    fill = NULL
  ) +
  theme(legend.position = "none")

ggsave(
  "png/clearance-crime-scatter.png",
  device = ragg::agg_png,
  dpi = 300,
  height = 6,
  width = 9,
  units = "in",
  bg = "white"
)


to_plot <- viol_crime_by_state |>
  filter(year == 2020, state != "Illinois") |>
  inner_join(viol_prison_state, by = "state") |>
  mutate(
    crime_rate = actual / pop_total,
    violent_prison_pop_rate = violent_n / pop_total
  )

a <- cor(to_plot$clearance_rate, to_plot$violent_prison_pop_rate)

a^2

cor.test(to_plot$clearance_rate, to_plot$violent_prison_pop_rate)

to_plot |>
  ggplot(aes(clearance_rate, violent_prison_pop_rate)) +
  geom_point(aes(size = pop_total), alpha = 0.5, color = jr_pal[3]) +
  geom_text_repel(
    data = filter(to_plot, state_abb %in% c("DC", "VT", "ID", "CA", "NY", "NM",
                                            "OH", "TX", "FL", "HI", "MS", "GA", "DE",
                                            "MA", "ME", "MN", "LA", "OK")),
    aes(label = state_abb),
    family = "Franklin Gothic Demi",
    size = 3.5
  ) +
  scale_x_continuous(labels = label_percent(1)) +
  scale_y_continuous(labels = label_number(scale = 1e5, big.mark = ",")) +
  scale_size(range = c(2, 10)) +
  # geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Prison population and clearance rate by state, 2019",
    subtitle = "Size of circles are proportionate to state population",
    x = "Violent crime clearance rate",
    y = "Prison population for violent offenses (per 100k)",
    fill = NULL
  ) +
  theme(legend.position = "none")

ggsave(
  "png/clearance-prison-pop-scatter.png",
  device = ragg::agg_png,
  dpi = 300,
  height = 6,
  width = 8,
  units = "in",
  bg = "white"
)

