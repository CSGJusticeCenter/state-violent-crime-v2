## Unsolved violent crime and homicide by state, 2020-2025
##
## Pools each state's reported and unsolved counts over the years the site
## shows a solve rate. Years hidden for low 2021 coverage or the clearance
## screen drop out of both counts, so a state missing a year shows a minimum.
## Violent crime uses the violent-total screen and homicide uses the homicide
## screen, which also drops New York's 2022 homicides.
##
## Writes analysis/crime_wo_conseq/state-2020-2025.csv. Run from the project root.

library(tidyverse)
library(csgjcr)
source("R/page-data.R")
source("analysis/crime_wo_conseq/helpers.R")

first_year <- 2020
last_year <- 2025
n_years <- last_year - first_year + 1

srs <- read_srs_state() |>
  filter(crime_cat == "Violent", between(year, first_year, last_year))
coverage <- read_solve_coverage()

### Violent crime --------------------------------------------------------------

violent_annual <- srs |>
  summarize(
    pop_total = sum(pop_total[group == "Homicide"]),
    across(c(incidents_reported, incidents_unsolved), sum),
    hide_total_solve = any(hide_total_solve),
    .by = c(year, state_name, state_abbr)
  ) |>
  mutate(hidden = solve_rate_hidden(pick(everything()), coverage, exclude = character()))

violent <- violent_annual |>
  summarize(
    years_shown = sum(!hidden),
    pop_years = sum(pop_total),
    reported_all = sum(incidents_reported),
    reported = sum(incidents_reported[!hidden]),
    unsolved = sum(incidents_unsolved[!hidden]),
    .by = c(state_name, state_abbr)
  )

### Homicide -------------------------------------------------------------------

homicide <- srs |>
  filter(group == "Homicide") |>
  mutate(hidden = solve_rate_hidden(pick(everything()), coverage, exclude = character())) |>
  summarize(
    homicide_years_shown = sum(!hidden),
    homicide_unsolved = sum(incidents_unsolved[!hidden]),
    .by = c(state_name, state_abbr)
  )

### State table ----------------------------------------------------------------

states <- violent |>
  left_join(homicide, by = join_by(state_name, state_abbr), relationship = "one-to-one") |>
  mutate(
    no_data = state_name %in% states_to_exclude_from_solved_rate_viz,
    year_missing = years_shown < n_years,
    homicide_year_missing = homicide_years_shown < n_years,
    across(c(unsolved, homicide_unsolved), \(x) if_else(no_data, NA_real_, x)),
    pct_unsolved = unsolved / reported,
    pct_solved = 1 - pct_unsolved,

    ### per 100k residents per year, over population summed across the six years
    reported_per_100k = reported_all / pop_years * 1e5
  )

stopifnot(nrow(states) == 51)

write_csv(states, state_path)
