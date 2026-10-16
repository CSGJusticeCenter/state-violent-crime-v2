## Unsolved violent crime by state, 2020-2025
##
## Pools each state's reported and unsolved counts over the years the site
## shows a solve rate. Years hidden for low 2021 coverage or the clearance
## screen drop out of both counts. Writes analysis/state-unsolved-2020-2025.csv

library(tidyverse)
library(csgjcr)
source("R/page-data.R")

first_year <- 2020
last_year <- 2025

srs <- read_srs_state()
coverage <- read_solve_coverage()

annual <- srs |>
  filter(crime_cat == "Violent", between(year, first_year, last_year)) |>
  summarize(
    pop_total = sum(pop_total[group == "Homicide"]),
    across(c(incidents_reported, incidents_unsolved), sum),
    hide_total_solve = any(hide_total_solve),
    .by = c(year, state_name, state_abbr)
  ) |>
  mutate(hidden = solve_rate_hidden(pick(everything()), coverage, exclude = character()))

states <- annual |>
  summarize(
    years_shown = sum(!hidden),
    pop_2025 = pop_total[year == last_year],
    reported_all = sum(incidents_reported),
    reported = sum(incidents_reported[!hidden]),
    unsolved = sum(incidents_unsolved[!hidden]),
    .by = c(state_name, state_abbr)
  ) |>
  mutate(
    year_missing = years_shown < last_year - first_year + 1,
    no_data = state_name %in% states_to_exclude_from_solved_rate_viz,
    pct_unsolved = if_else(no_data, NA_real_, unsolved / reported),
    pct_solved = 1 - pct_unsolved,

    ### unsolved crimes scaled to all six years, per 100k residents per year
    unsolved_all_years = pct_unsolved * reported_all,
    unsolved_per_100k = unsolved_all_years / (last_year - first_year + 1) / pop_2025 * 1e5
  )

write_csv(states, "analysis/state-unsolved-2020-2025.csv")

states |>
  arrange(desc(pct_unsolved)) |>
  select(state_abbr, years_shown, pct_unsolved, unsolved_all_years, unsolved_per_100k) |>
  print(n = Inf)
