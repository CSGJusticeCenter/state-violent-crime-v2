library(tidyverse)
library(csgjcr)

sp_data_analysis_path <- csg_sp_path("jr_data_library", "data", "analysis")
sp_data_to_plot_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")

srs <- read_rds(file.path(sp_data_analysis_path, "fbi", "srs", "fbi_srs_state.rds")) |>
  mutate(
    crime_cat = if_else(
      group %in% c("Aggravated assault", "Homicide", "Rape", "Robbery"),
      "Violent", "Property")
    )

srs_state <- srs |>
  filter(indicator != "Unsolved rate") |>
  pivot_wider(
    id_cols = c(year, state_name, state_abbr, group, crime_cat, pop_total, pop_adult),
    names_from = indicator, values_from = n
  ) |>
  janitor::clean_names() |>
  mutate(
    pct_unsolved = incidents_unsolved / incidents_reported,
    incidents_reported_rate_total = incidents_reported / pop_total,
    incidents_reported_rate_adult = incidents_reported / pop_adult
  )

srs_by_cat_state <- srs |>
  group_by(year, state_name, state_abbr, state_fips, indicator, crime_cat,
           pop_total, pop_adult) |>
  summarize(n = sum(n)) |>
  ungroup() |>
  filter(indicator != "Unsolved rate") |>
  pivot_wider(
    id_cols = c(year, state_name, state_abbr, crime_cat, pop_total, pop_adult),
    names_from = indicator, values_from = n
  ) |>
  janitor::clean_names() |>
  mutate(
    pct_unsolved = incidents_unsolved / incidents_reported,
    incidents_reported_rate_total = incidents_reported / pop_total,
    incidents_reported_rate_adult = incidents_reported / pop_adult
  )

srs_us <- srs |>
  group_by(year, state_name = "United States", state_abbr = "US", indicator, crime_cat, group) |>
  summarize(across(c(n, pop_total, pop_adult), \(x) sum(x, na.rm = TRUE))) |>
  ungroup() |>
  filter(indicator != "Unsolved rate") |>
  pivot_wider(
    id_cols = c(year, state_name, state_abbr, group, crime_cat, pop_total, pop_adult),
    names_from = indicator, values_from = n
    ) |>
  janitor::clean_names() |>
  mutate(
    pct_unsolved = incidents_unsolved / incidents_reported,
    incidents_reported_rate_total = incidents_reported / pop_total,
    incidents_reported_rate_adult = incidents_reported / pop_adult
  )

srs_by_cat_us <- srs_us |>
  group_by(year, state_name, state_abbr, crime_cat,
           pop_total, pop_adult) |>
  summarize(
    across(c(incidents_reported, incidents_cleared, incidents_unsolved),
           \(x) sum(x, na.rm = TRUE))
    ) |>
  ungroup() |>
  mutate(
    pct_unsolved = incidents_unsolved / incidents_reported,
    incidents_reported_rate_total = incidents_reported / pop_total,
    incidents_reported_rate_adult = incidents_reported / pop_adult
  )

write_rds(srs_state, file.path(sp_data_to_plot_path, "srs_state.rds"))
write_rds(srs_by_cat_state, file.path(sp_data_to_plot_path, "srs_by_cat_state.rds"))
write_rds(srs_us, file.path(sp_data_to_plot_path, "srs_us.rds"))
write_rds(srs_by_cat_us, file.path(sp_data_to_plot_path, "srs_by_cat_us.rds"))
