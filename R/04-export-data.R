## write US and state csvs of reported, solved and unsolved violent crime and
## solve rates by year and offense, matching the numbers on the site pages

library(tidyverse)
library(csgjcr)

source("R/page-data.R")

export_dir <- "exports"

srs <- read_srs_state()
latest_year <- max(srs$year)
first_year <- latest_year - 10
solve_coverage <- read_solve_coverage()

states <- sort(unique(as.character(srs$state_name)))

### one row per year and offense, with the violent crime total first
### solved counts round to whole incidents and unsolved counts fill the rest
### solve rates are blank where the site hides them
format_export <- function(df) {
  df |>
    mutate(
      offense = factor(group, c(total_label, violent_offenses)),
      reported = round(incidents_reported),
      solved = round(incidents_solved),
      unsolved = reported - solved,
      solve_rate = pct_solved,
      hide_solved = low_solve_coverage(pick(year, state_name), solve_coverage) |
        screened_out(pick(everything())) |
        state_name %in% states_to_exclude_from_solved_rate_viz,
      across(c(solved, unsolved, solve_rate), \(x) if_else(hide_solved, NA, x))
    ) |>
    arrange(state_name, year, offense) |>
    select(year, state_name, state_abbr, offense, reported, solved, unsolved, solve_rate)
}

### US solve rates pool every state but the excluded ones, as on the US page
us <- bind_rows(
  us_solve_trend(srs, first_year) |> mutate(group = total_label),
  us_solve_trend(srs, first_year, by_offense = TRUE)
) |>
  format_export() |>
  select(-state_name, -state_abbr)

state <- bind_rows(
  map(states, \(s) violent_totals(srs, first_year, s)) |>
    list_rbind() |>
    mutate(group = total_label),
  map(states, \(s) violent_by_offense(srs, first_year, s)) |>
    list_rbind()
) |>
  format_export()

dir.create(export_dir, showWarnings = FALSE)
write_csv(us, file.path(export_dir, "violent-crime-solve-rates-us.csv"), na = "")
write_csv(state, file.path(export_dir, "violent-crime-solve-rates-states.csv"), na = "")
