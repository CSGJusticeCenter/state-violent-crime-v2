## Shared paths, reads and helpers for the handout scripts
##
## Needs tidyverse attached. Run from the project root.

handout_dir <- "analysis/crime_wo_conseq"
national_path <- file.path(handout_dir, "national-1965-2025.csv")
state_path <- file.path(handout_dir, "state-2020-2025.csv")

read_national <- function() read_csv(national_path, show_col_types = FALSE)

read_states <- function() read_csv(state_path, show_col_types = FALSE)

### national rows for the handout's 2020-2025 period
recent_years <- function(national) filter(national, between(year, 2020, 2025))

### US solve rate, 2020-2025, from the national series, as on pages 1, 3 and 4
pooled_solve_rate <- function(recent) sum(recent$incidents_solved) / sum(recent$incidents_reported)

### US violent crimes per 100k residents per year, 2020-2025, over population
### summed across the years
pooled_crime_rate <- function(states) sum(states$reported_all) / sum(states$pop_years) * 1e5

### periods of `width` years from the first year in df, with yearly averages of
### reported and solved and the rate pooled as total solved over total reported
periods <- function(df, width) {
  first_year <- min(df$year)

  out <- df |>
    mutate(start = first_year + (year - first_year) %/% width * width) |>
    summarize(
      end = max(year),
      years = n(),
      across(c(incidents_reported, incidents_solved), sum),
      .by = start
    ) |>
    mutate(
      pct_solved = incidents_solved / incidents_reported,
      label = str_c(start, "–", str_sub(end, 3)),
      across(c(incidents_reported, incidents_solved), \(x) x / years),
      incidents_unsolved = incidents_reported - incidents_solved
    )

  if (any(out$years != width)) stop("Every period needs ", width, " years")
  out
}

### share solved as the map and table print it, with * for a missing year
solved_label <- function(pct, partial_years) str_c(round(100 * pct), "%", if_else(partial_years, "*", ""))
