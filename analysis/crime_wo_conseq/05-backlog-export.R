## US backlog of unsolved violent crime by year, 2020-2025, in total and by offense
##
## Backlog is unsolved crime summed from 2020 through each year. The total comes
## from the national series, as in the page 1 backlog chart. Each offense has its
## own pooled US solve rate, as in page 1's offense counts, so the offense rows
## add up to slightly more than the total.
##
## Writes analysis/crime_wo_conseq/us-backlog-2020-2025.csv. Reads the CSV from
## 01-national-series.R. Run from the project root.

library(tidyverse)
library(csgjcr)
source("R/page-data.R")
source("analysis/crime_wo_conseq/helpers.R")

total <- read_national() |>
  recent_years() |>
  transmute(offense = "Violent crime", year, incidents_reported, incidents_solved, incidents_unsolved)

by_offense <- us_solve_trend(read_srs_state(), 2020, by_offense = TRUE) |>
  filter(year <= 2025) |>
  transmute(offense = as.character(group), year, incidents_reported, incidents_solved, incidents_unsolved)

backlog <- bind_rows(total, by_offense) |>
  arrange(year) |>
  mutate(
    pct_solved = incidents_solved / incidents_reported,
    backlog = cumsum(incidents_unsolved),
    .by = offense
  ) |>
  mutate(offense = fct_relevel(offense, "Violent crime", "Homicide", "Aggravated assault", "Robbery", "Rape")) |>
  arrange(offense, year)

if (any(count(backlog, offense)$n != 6)) stop("Every offense needs six years")

write_csv(backlog, file.path(handout_dir, "us-backlog-2020-2025.csv"))

backlog |>
  filter(year == 2025) |>
  select(offense, backlog) |>
  print()
