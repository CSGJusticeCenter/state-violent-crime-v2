## graphics for 2024 ucr update
## request from jess: I'm thinking four graphics in total: (1) violent crime rate 2019-2024, (2) violent crime rate 2023-2024, (3) violent crime solve rates 2019-2024, (4) violent crime solve rates 2023-2024.

library(tidyverse)
library(csgjcr)

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


viol_crime_state |> 
  filter(year == 2023) |> 
  mutate(rank = dense_rank(desc(incidents_per_100k))) |> 
  arrange(rank) 