## State backlog of unsolved violent crime by year, 2020-2025, in total and by offense
##
## Each row pools a state's counts from 2020 through its year, so the 2025 rows
## hold the 2020-2025 state table. Unsolved counts and backlogs use each state's
## own clearances and count only the years the site shows a solve rate. Years
## hidden for low 2021 coverage or the clearance screen drop out of the pooled
## reported and unsolved counts, so a state missing a year shows a minimum.
## Violent crime uses the violent-total screen and each offense its own screen,
## which also drops New York's 2022 homicides. Excluded states keep reported
## counts but no unsolved counts.
##
## Writes analysis/crime_wo_conseq/state-backlog-2020-2025.csv. Run from the
## project root.

library(tidyverse)
library(csgjcr)
source("R/page-data.R")
source("analysis/crime_wo_conseq/helpers.R")

first_year <- 2020
last_year <- 2025

srs <- read_srs_state() |>
  filter(crime_cat == "Violent", between(year, first_year, last_year)) |>
  mutate(across(c(state_name, state_abbr, group), as.character))
coverage <- read_solve_coverage()

### homicide rows carry each state's population once
population <- srs |>
  filter(group == "Homicide") |>
  select(year, state_name, population = pop_total)

### Yearly counts --------------------------------------------------------------

violent <- srs |>
  summarize(
    across(c(incidents_reported, incidents_unsolved), sum),
    hide_total_solve = any(hide_total_solve),
    .by = c(year, state_name, state_abbr)
  ) |>
  mutate(
    offense = "Violent crime",
    hidden = solve_rate_hidden(pick(everything()), coverage, exclude = character())
  )

by_offense <- srs |>
  mutate(
    offense = group,
    hidden = solve_rate_hidden(pick(everything()), coverage, exclude = character())
  )

yearly <- bind_rows(violent, by_offense) |>
  select(state_name, state_abbr, offense, year, incidents_reported, incidents_unsolved, hidden) |>
  left_join(population, by = join_by(year, state_name), relationship = "many-to-one") |>
  mutate(
    no_data = state_name %in% states_to_exclude_from_solved_rate_viz,
    incidents_unsolved = if_else(hidden | no_data, NA_real_, incidents_unsolved),
    incidents_solved = incidents_reported - incidents_unsolved
  )

### Backlog --------------------------------------------------------------------

backlog <- yearly |>
  arrange(year) |>
  mutate(
    years_shown = cumsum(!hidden),
    pop_years = cumsum(population),
    reported_all = cumsum(incidents_reported),
    reported = cumsum(if_else(hidden, 0, incidents_reported)),
    backlog = if_else(no_data, NA_real_, cumsum(coalesce(incidents_unsolved, 0))),
    .by = c(state_name, offense)
  ) |>
  mutate(
    year_missing = years_shown < year - first_year + 1,
    pct_solved = if_else(reported > 0, 1 - backlog / reported, NA_real_),

    ### per 100k residents per year, over population summed across the years
    reported_per_100k = reported_all / pop_years * 1e5,
    offense = fct_relevel(offense, "Violent crime", "Homicide", "Aggravated assault", "Robbery", "Rape")
  ) |>
  select(
    state_name, state_abbr, offense, year, population,
    incidents_reported, incidents_solved, incidents_unsolved, hidden,
    years_shown, pop_years, reported_all, reported, backlog, pct_solved, reported_per_100k,
    no_data, year_missing
  ) |>
  arrange(state_name, offense, year)

if (nrow(backlog) != 51 * 5 * (last_year - first_year + 1)) stop("Every state needs six years of five series")
if (anyNA(backlog$population)) stop("Some state-years lack population")

write_csv(backlog, state_path)
