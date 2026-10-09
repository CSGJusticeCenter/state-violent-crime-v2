library(testthat)
library(tidyverse)

source("../R/page-data.R")

### two states, one year, homicide and robbery plus one property offense
raw_srs <- function() {
  expand_grid(
    year = 2025,
    state = c("AA", "BB"),
    group = c("Homicide", "Robbery", "Burglary"),
    indicator = c("Incidents reported", "Incidents unsolved", "Incidents cleared", "Unsolved rate")
  ) |>
    mutate(
      state_name = if_else(state == "AA", "State A", "State B"),
      state_abbr = state,
      state_fips = "00", group_cat = "offense_type",
      rate_total = NA_real_, rate_adult = NA_real_,
      pop_total = if_else(state == "AA", 1000, 3000), pop_adult = 800,
      n = case_when(
        indicator == "Incidents reported" & group == "Homicide" ~ 10,
        indicator == "Incidents reported" ~ 40,
        indicator == "Incidents unsolved" ~ 4,
        indicator == "Incidents cleared" ~ 6,
        TRUE ~ 0.1
      )
    ) |>
    select(-state)
}

test_that("SRS prep pivots indicators and drops unused columns", {
  srs <- prep_srs_state(raw_srs())
  expect_named(srs, c(
    "year", "state_name", "state_abbr", "group", "crime_cat", "pop_total",
    "incidents_reported", "incidents_unsolved", "incidents_cleared",
    "incidents_reported_rate_total"
  ))
  expect_equal(nrow(srs), 6)
  expect_equal(srs$crime_cat[srs$group == "Burglary"], c("Property", "Property"))
  expect_equal(srs$incidents_reported_rate_total[1], 10 / 1000)
})

test_that("violent totals sum offenses and count population once", {
  srs <- prep_srs_state(raw_srs())

  state <- violent_totals(srs, 2015, "State A")
  expect_equal(state$pop_total, 1000)
  expect_equal(state$incidents_reported, 50)
  expect_equal(state$pct_solved, 42 / 50)

  us <- violent_totals(srs, 2015)
  expect_equal(us$state_name, "United States")
  expect_equal(us$state_abbr, "U.S.")
  expect_equal(us$pop_total, 4000)
  expect_equal(us$incidents_reported_rate_total, 100 / 4000)
  expect_false(dplyr::is_grouped_df(us))
})

test_that("violent offenses keep state rows and pool US rows", {
  srs <- prep_srs_state(raw_srs())

  state <- violent_by_offense(srs, 2015, "State B")
  expect_equal(state$group, c("Homicide", "Robbery"))
  expect_equal(state$incidents_solved, c(6, 36))

  us <- violent_by_offense(srs, 2015)
  expect_equal(us$group, c("Homicide", "Robbery"))
  expect_equal(us$pop_total, c(4000, 4000))
  expect_equal(us$pct_solved, c(12 / 20, 72 / 80))
})

test_that("inflation adjustment uses the latest year's dollars", {
  spending <- tibble(year = c(2023, 2024), n = c(100, 100))
  index <- tibble(year = c(2023, 2024), inflation_index = c(100, 110))
  adjusted <- adjust_for_inflation(spending, index)
  expect_equal(adjusted$n_inflation_adjusted_latest_year_dollars, c(110, 100))
})

test_that("officer rates scale the covered rate by residents and violent crime", {
  lee <- tibble(rate_covered = 0.002, violent_rate = 0.004)
  rates <- add_officer_rates(lee)
  expect_equal(rates$officer_pop_rate_per_10k, 20)
  expect_equal(rates$officer_per_violent_offense_1k, 500)
})
