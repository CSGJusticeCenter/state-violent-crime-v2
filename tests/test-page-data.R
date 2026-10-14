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

test_that("offense rate benchmark gives each offense's rate per 100k in one year", {
  srs <- prep_srs_state(raw_srs())

  ### US: homicide 20 and robbery 80 incidents among 4,000 residents
  us <- offense_rate_benchmark(violent_by_offense(srs, 2015), 2025)
  expect_equal(us, c(homicide = 500, robbery = 2000))

  expect_error(offense_rate_benchmark(violent_by_offense(srs, 2015), 2019), "No 2019")
})

test_that("violent rows keep violent offenses from first_year on", {
  srs <- prep_srs_state(bind_rows(raw_srs(), mutate(raw_srs(), year = 2014)))

  rows <- violent_rows(srs, 2015, "State A")
  expect_equal(unique(rows$year), 2025)
  expect_equal(sort(unique(rows$group)), c("Homicide", "Robbery"))
  expect_equal(unique(as.character(rows$state_name)), "State A")

  us <- violent_rows(srs, 2014)
  expect_equal(sort(unique(us$year)), c(2014, 2025))
  expect_equal(unique(us$state_name), "United States")
})

test_that("US solve benchmark pools every state except the excluded ones", {
  srs <- prep_srs_state(raw_srs()) |>
    mutate(incidents_unsolved = if_else(state_abbr == "BB", 0, incidents_unsolved))

  ### State A alone: homicide 6/10 solved, robbery 36/40
  benchmark <- us_solve_benchmark(srs, 2025, exclude = "State B")
  expect_named(benchmark, c("homicide", "robbery"))
  expect_equal(unname(benchmark), c(6 / 10, 36 / 40))

  ### both states pooled: State B solves everything
  pooled <- us_solve_benchmark(srs, 2025, exclude = character())
  expect_equal(unname(pooled), c(16 / 20, 76 / 80))

  expect_equal(states_to_exclude_from_solved_rate_viz, "Illinois")
})

test_that("solve rate pool drops excluded states from US solve totals", {
  srs <- prep_srs_state(raw_srs()) |>
    mutate(incidents_unsolved = if_else(state_abbr == "BB", 0, incidents_unsolved))

  expect_equal(unique(solve_rate_pool(srs, exclude = "State B")$state_name), "State A")
  expect_equal(sort(unique(solve_rate_pool(srs, exclude = character())$state_name)), c("State A", "State B"))
  expect_equal(nrow(solve_rate_pool(srs)), nrow(srs))

  ### State A alone: 42 of 50 violent incidents solved
  us <- violent_totals(solve_rate_pool(srs, exclude = "State B"), 2015)
  expect_equal(us$state_name, "United States")
  expect_equal(us$incidents_reported, 50)
  expect_equal(us$incidents_solved, 42)
  expect_equal(us$pct_solved, 42 / 50)

  by_offense <- violent_by_offense(solve_rate_pool(srs, exclude = "State B"), 2015)
  benchmark <- us_solve_benchmark(srs, 2025, exclude = "State B")
  expect_equal(by_offense$pct_solved, unname(benchmark))
})

test_that("US solve trend applies the pooled solve rate to every state's reports", {
  srs <- prep_srs_state(raw_srs()) |>
    mutate(incidents_unsolved = if_else(state_abbr == "BB", 0, incidents_unsolved))

  ### State A solves 42 of 50; both states report 100
  us <- us_solve_trend(srs, 2015, exclude = "State B")
  expect_equal(us$state_name, "United States")
  expect_equal(us$incidents_reported, 100)
  expect_equal(us$pct_solved, 42 / 50)
  expect_equal(us$incidents_solved, 84)
  expect_equal(us$incidents_unsolved, 16)

  by_offense <- us_solve_trend(srs, 2015, by_offense = TRUE, exclude = "State B")
  expect_equal(by_offense$group, c("Homicide", "Robbery"))
  expect_equal(by_offense$incidents_reported, c(20, 80))
  expect_equal(by_offense$pct_solved, c(6 / 10, 36 / 40))
  expect_equal(by_offense$incidents_solved, c(12, 72))
})

test_that("US solve trend matches each year's pooled rate to that year", {
  ### in 2024 State A solves everything; in 2025 it solves 42 of 50
  srs <- prep_srs_state(bind_rows(raw_srs(), mutate(raw_srs(), year = 2024))) |>
    mutate(incidents_unsolved = if_else(state_abbr == "AA" & year == 2024, 0, incidents_unsolved))

  us <- us_solve_trend(srs, 2015, exclude = "State B") |> arrange(year)
  expect_equal(us$year, c(2024, 2025))
  expect_equal(us$pct_solved, c(1, 42 / 50))
  expect_equal(us$incidents_solved, c(100, 84))

  by_offense <- us_solve_trend(srs, 2015, by_offense = TRUE, exclude = "State B") |>
    arrange(year, group)
  expect_equal(by_offense$pct_solved, c(1, 1, 6 / 10, 36 / 40))
})

test_that("US solve trend stops when a pooled rate is missing", {
  srs <- prep_srs_state(raw_srs())
  expect_error(us_solve_trend(srs, 2015, exclude = c("State A", "State B")), "pooled solve rate")
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

test_that("2021 solve rates drop below the coverage bar and keep the rest", {
  coverage <- tibble(
    state_name = c("State A", "State B", "United States"),
    coverage = c(0.5, 0.75, 0.77)
  )
  df <- tibble(
    year = c(2020, 2021, 2021, 2021, 2022),
    state_name = factor(c("State A", "State A", "State B", "United States", "State A"))
  )

  expect_equal(low_solve_coverage(df, coverage), c(FALSE, TRUE, FALSE, FALSE, FALSE))

  kept <- solve_rate_years(df, coverage)
  expect_equal(kept$year, c(2020, 2021, 2021, 2022))
  expect_equal(as.character(kept$state_name), c("State A", "State B", "United States", "State A"))
  expect_equal(kept$coverage_note[kept$year != 2021], c("", ""))
  expect_equal(
    kept$coverage_note[kept$state_name == "State B"],
    "<br>Agencies covering 75% of the population reported in 2021"
  )
})

test_that("2021 solve rates need coverage for every row in that year", {
  coverage <- tibble(state_name = "State A", coverage = 0.9)
  df <- tibble(year = c(2020, 2021), state_name = c("State B", "State B"))
  expect_error(solve_rate_years(df, coverage), "Missing 2021 coverage")
  expect_equal(low_solve_coverage(df[1, ], coverage), FALSE)
})

### State A and State B in 2022 and 2025
two_year_srs <- function() {
  prep_srs_state(bind_rows(raw_srs(), mutate(raw_srs(), year = 2022)))
}

### the screen fails one state in 2022 and passes 2025
screen_2022 <- function(state = "State B", share = 0.5) {
  tibble(year = c(2022, 2025), state_name = state, share_no_clearances = c(share, 0))
}

repairs_2022 <- function(state = "State B", rate = 0.5) {
  tibble(year = 2022, state_name = state, group = violent_offenses, pct_cleared = rate)
}

no_repairs <- tibble(year = numeric(), state_name = character(), group = character(), pct_cleared = numeric())

test_that("clearance repairs replace cleared and unsolved counts for matching rows", {
  srs <- repair_clearances(two_year_srs(), repairs_2022())
  fixed <- filter(srs, year == 2022, state_name == "State B", group == "Robbery")
  expect_equal(fixed$incidents_cleared, 20)
  expect_equal(fixed$incidents_unsolved, 20)

  kept <- filter(srs, year == 2025, state_name == "State B", group == "Robbery")
  expect_equal(kept$incidents_cleared, 6)
  expect_equal(kept$incidents_unsolved, 4)
})

test_that("screened state-years without repairs hide every solve rate", {
  srs <- flag_screened_solve_rates(two_year_srs(), screen_2022(), no_repairs)
  hidden <- filter(srs, year == 2022, state_name == "State B", crime_cat == "Violent")
  expect_true(all(hidden$hide_total_solve))
  expect_true(all(hidden$hide_offense_solve))
  expect_false(any(filter(srs, state_name == "State A")$hide_offense_solve))
})

test_that("screen shares under the bar hide nothing", {
  srs <- flag_screened_solve_rates(two_year_srs(), screen_2022(share = 0.01), no_repairs)
  expect_false(any(srs$hide_total_solve))
})

test_that("repaired state-years hide only offenses outside the shown list", {
  srs <- flag_screened_solve_rates(two_year_srs(), screen_2022(), repairs_2022())
  b_2022 <- filter(srs, year == 2022, state_name == "State B")
  expect_false(any(b_2022$hide_total_solve))
  expect_equal(b_2022$hide_offense_solve[b_2022$group == "Homicide"], TRUE)
  expect_equal(b_2022$hide_offense_solve[b_2022$group == "Robbery"], FALSE)
})

test_that("repairs must cover every violent offense", {
  expect_error(
    flag_screened_solve_rates(two_year_srs(), screen_2022(), repairs_2022()[1:2, ]),
    "every violent offense"
  )
})

test_that("the screen stops when it hides a compared year", {
  screen <- tibble(year = c(2022, 2025), state_name = "State B", share_no_clearances = c(0, 0.5))
  expect_error(flag_screened_solve_rates(two_year_srs(), screen, no_repairs), "State B 2025")
  expect_no_error(flag_screened_solve_rates(two_year_srs(), mutate(screen, state_name = "Illinois"), no_repairs))
})

test_that("the screen stops when a shown year has no screen rows", {
  screen <- filter(screen_2022(), year == 2022)
  expect_error(flag_screened_solve_rates(two_year_srs(), screen, no_repairs), "no rows for 2025")
})

test_that("US solve rates pool unscreened states and US counts keep every state", {
  srs <- flag_screened_solve_rates(two_year_srs(), screen_2022(), no_repairs)
  us <- filter(us_solve_trend(srs, 2015, exclude = character()), year == 2022)

  ### State A alone sets the rate, 42 of 50 solved, applied to both states' 100 crimes
  expect_equal(us$pct_solved, 42 / 50)
  expect_equal(us$incidents_reported, 100)
  expect_equal(us$incidents_solved, 84)
})

test_that("US offense solve rates drop hidden offenses and keep repaired ones", {
  srs <- two_year_srs() |>
    repair_clearances(repairs_2022()) |>
    flag_screened_solve_rates(screen_2022(), repairs_2022())
  us <- filter(us_solve_trend(srs, 2015, by_offense = TRUE, exclude = character()), year == 2022)

  ### homicide uses State A's 6 of 10, and robbery pools A's 36 and B's repaired 20 of 80
  expect_equal(us$pct_solved[us$group == "Homicide"], 6 / 10)
  expect_equal(us$incidents_reported[us$group == "Homicide"], 20)
  expect_equal(us$pct_solved[us$group == "Robbery"], 56 / 80)
})

test_that("solve_rate_hidden marks what solve_rate_years drops plus excluded states", {
  srs <- flag_screened_solve_rates(two_year_srs(), screen_2022(), no_repairs)
  offenses <- bind_rows(violent_by_offense(srs, 2015, "State A"), violent_by_offense(srs, 2015, "State B"))
  coverage <- tibble(state_name = c("State A", "State B"), coverage = 1)

  kept <- solve_rate_years(offenses, coverage)
  expect_equal(nrow(kept), sum(!solve_rate_hidden(offenses, coverage, exclude = character())))
  expect_equal(sum(solve_rate_hidden(offenses, coverage, exclude = character())), 2)
  expect_true(all(solve_rate_hidden(offenses, coverage, exclude = "State A")[offenses$state_name == "State A"]))
})

test_that("screened rows leave the US pools and the trend rows", {
  srs <- flag_screened_solve_rates(two_year_srs(), screen_2022(), repairs_2022())

  ### State B 2022 stays in the total pool but leaves the homicide pool
  expect_equal(nrow(solve_rate_pool(srs, exclude = character())), nrow(srs))
  by_offense_pool <- solve_rate_pool(srs, exclude = character(), by_offense = TRUE)
  expect_false(any(by_offense_pool$year == 2022 & by_offense_pool$state_name == "State B" & by_offense_pool$group == "Homicide"))

  totals <- violent_totals(srs, 2015, "State B")
  expect_equal(totals$hide_total_solve, c(FALSE, FALSE))
  offenses <- violent_by_offense(srs, 2015, "State B")
  expect_equal(offenses$year[screened_out(offenses)], 2022)
  expect_equal(offenses$group[screened_out(offenses)], "Homicide")
})

test_that("screened state-years drop from totals and offense trend rows", {
  srs <- flag_screened_solve_rates(two_year_srs(), screen_2022(), no_repairs)
  coverage <- tibble(state_name = c("State A", "State B", "United States"), coverage = 1)

  totals <- violent_totals(srs, 2015, "State B")
  expect_equal(solve_rate_years(totals, coverage)$year, 2025)

  offenses <- violent_by_offense(srs, 2015, "State B")
  expect_equal(unique(solve_rate_years(offenses, coverage)$year), 2025)

  us <- violent_totals(srs, 2015)
  expect_false("hide_total_solve" %in% names(us))
  expect_false(any(screened_out(us)))
})

test_that("state benchmarks use the state estimates and blank screened offenses", {
  srs <- prep_srs_state(raw_srs())
  benchmark <- state_solve_benchmark(srs, "State A", 2025)
  expect_named(benchmark, c("homicide", "robbery"))
  expect_equal(unname(benchmark), c(6 / 10, 36 / 40))

  screened <- mutate(srs, hide_offense_solve = group == "Robbery")
  expect_true(is.na(state_solve_benchmark(screened, "State A", 2025)[["robbery"]]))
  expect_error(state_solve_benchmark(srs, "State A", 2030), "No 2030 solve rates")
})
