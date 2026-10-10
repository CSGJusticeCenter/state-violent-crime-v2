library(testthat)
library(dplyr)

source("../R/local-map-data.R")
source("../R/page-data.R")
source("../R/agency-county-data.R")

row <- function(ori, year, group, indicator, n, county_fips, status = "full", pop = 1000,
                method = "single county", fips_all = county_fips, state = "NY") {
  tibble(
    year = year, state_name = state, state_abbr = state, ori = ori, agency_name = ori,
    county_fips = county_fips, county_name = "X", county_fips_all = fips_all,
    county_method = method, indicator = indicator, group = group, n = n,
    pop_covered = pop, reporting_status = status
  )
}

agency <- function(ori, year, reported, cleared, ...) {
  bind_rows(
    row(ori, year, "Homicide", "Incidents reported", reported, ...),
    row(ori, year, "Homicide", "Incidents cleared", cleared, ...),
    row(ori, year, "Burglary", "Incidents reported", 99, ...)
  )
}

raw <- bind_rows(
  agency("A", 2025, 10, 5, "36001"),
  agency("A", 2019, 10, 2, "36001"),
  agency("B", 2025, 10, 10, "36047"),
  agency("C", 2025, 30, 15, "36061"),
  agency("D", 2025, 8, 4, "36003", status = "partial"),
  agency("E", 2025, 6, 3, NA, method = "statewide", fips_all = NA),
  agency("F", 2025, 4, 2, "36001", fips_all = "36001, 36003")
)

built <- build_agency_county_data(raw, 2025, 2019)

test_that("only full-reporting violent rows are counted", {
  expect_false("D" %in% built$agency_table$ori)
  expect_equal(built$agency_table$n_reported[built$agency_table$ori == "A"], 10)
})

test_that("solve rate change compares an agency with itself", {
  at <- built$agency_table
  expect_equal(at$solved_rate_change[at$ori == "A"], (0.5 - 0.2) * 100)
  expect_true(is.na(at$solved_rate_change[at$ori == "B"]))
  expect_equal(unique(at$base_year), 2019)
  expect_equal(unique(at$agency_year), 2025)
})

test_that("statewide agencies stay in the table but off the map", {
  expect_true("E" %in% built$agency_table$ori)
  expect_equal(built$agency_table$county[built$agency_table$ori == "E"], "Statewide")
  expect_false(any(is.na(built$county_map$county_fips)))
})

test_that("each agency is counted in exactly one county", {
  expect_equal(sum(built$county_map$n_reported), sum(built$agency_table$n_reported[!is.na(built$agency_table$county_fips)]))
  expect_equal(built$county_map$n_agencies[built$county_map$county_fips == "36001"], 2L)
})

test_that("borough agencies sum into one NYC row", {
  nyc <- built$county_map[built$county_map$county_fips == nyc_fips, ]
  expect_equal(nrow(nyc), 1L)
  expect_equal(nyc$n_reported, 40)
  expect_equal(nyc$n_solved, 25)
  expect_equal(nyc$n_agencies, 2L)
  expect_false(any(built$county_map$county_fips %in% nyc_borough_fips))
})

test_that("offense table carries counts and rates", {
  ao <- built$agency_offense_table
  expect_equal(ao$n_solved_homicide[ao$ori == "A"], 5)
  expect_equal(ao$solved_rate_homicide[ao$ori == "A"], 0.5)
  expect_true(is.na(ao$solved_rate_robbery[ao$ori == "A"]))
})

test_that("county suffixes are dropped from table names", {
  raw <- tibble(
    county_fips = c("09110", "02110", "02020"),
    county_name = c("Capitol Planning Region", "Juneau City and Borough", "Anchorage Municipality")
  )
  lookup <- county_name_lookup(raw)
  expect_equal(
    county_names_from_fips("09110, 02110, 02020", lookup),
    "Capitol, Juneau, Anchorage"
  )
})

test_that("reporting coverage counts agencies with at least one month", {
  coverage_raw <- bind_rows(
    agency("A", 2025, 10, 5, "36001", pop = 500),
    agency("D", 2025, 8, 4, "36003", status = "partial", pop = 300),
    agency("G", 2025, NA, NA, "36005", status = "none", pop = 900),
    agency("A", 2024, 10, 5, "36001", pop = 9999)
  )
  pep <- tibble(state_abbr = c("NY", "VT"), state_name = c("New York", "Vermont"),
                pop_total = c(1000, 600))
  coverage <- state_reporting_coverage(coverage_raw, pep, 2025)

  expect_equal(coverage$pop_reporting, c(800, 0))
  expect_equal(coverage$coverage, c(0.8, 0))
  expect_equal(coverage$state_name, c("New York", "Vermont"))
  expect_equal(unique(coverage$year), 2025)
})

test_that("mismatched rows for one agency-offense fail loudly", {
  bad <- bind_rows(
    row("A", 2025, "Homicide", "Incidents reported", 10, "36001", pop = 1000),
    row("A", 2025, "Homicide", "Incidents cleared", 5, "36001", pop = NA)
  )
  expect_error(build_agency_county_data(bad, 2025, 2019), "duplicate|missing")
})
