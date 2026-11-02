library(testthat)
library(highcharter)
library(reactable)
library(tidyverse)

source("../R/page-data.R")
source("../R/utils.R")

city_agency <- function(ori, state = "WA", type = "City", population = 500000, reported = 100,
                        solved = 40, flag = NA_character_, name = paste(ori, "Police Department")) {
  tibble(
    state_name = state_name_from_abbr(state), state_abbr = factor(state), ori = ori,
    agency_name = name, agency_type = type, population = population,
    n_reported = reported, n_reported_rate = reported / population * 1e5, n_solved = solved,
    solved_rate = if (is.na(flag)) solved / reported else NA_real_,
    n_reported_base = reported, solved_rate_change = 5, data_flag = flag
  )
}

city_offenses <- function(ori, homicide = c(20, 15), rape = c(10, 2)) {
  tibble(
    ori = ori,
    n_reported_homicide = homicide[1], n_solved_homicide = homicide[2],
    n_reported_rape = rape[1], n_solved_rape = rape[2],
    n_reported_robbery = 30, n_solved_robbery = 9,
    n_reported_aggravated_assault = 40, n_solved_aggravated_assault = 14,
    n_reported_rate_homicide = 4, n_reported_rate_rape = 2,
    n_reported_rate_robbery = 6, n_reported_rate_aggravated_assault = 8,
    solved_rate_homicide = homicide[2] / homicide[1], solved_rate_rape = rape[2] / rape[1],
    solved_rate_robbery = 0.3, solved_rate_aggravated_assault = 0.35
  )
}

agencies <- bind_rows(
  city_agency("SEA", solved = 33),
  city_agency("NYC", state = "NY", population = 8e6, solved = 69),
  city_agency("SMALL", population = 100000),
  city_agency("SHERIFF", type = "County"),
  city_agency("CHI", state = "IL"),
  city_agency("FLAG", flag = "no_violent_crime"),
  city_agency("GLN", state = "AZ", name = "Glendale Police Department"),
  city_agency("GLC", state = "CA", name = "Glendale Police Department", solved = 60)
)
offenses <- map(agencies$ori, city_offenses) |> list_rbind()
offenses$n_reported_homicide[offenses$ori == "SEA"] <- 5

rows <- city_solve_rows(agencies, offenses)
long <- city_solve_long(rows)

test_that("city rows keep large city agencies outside excluded states", {
  expect_setequal(rows$ori, c("SEA", "NYC", "GLN", "GLC"))
  expect_equal(rows$ori[1], "NYC")
  expect_type(rows$state_abbr, "character")
})

test_that("excluded agencies are left out by ORI", {
  expect_false("SEA" %in% city_solve_rows(agencies, offenses, exclude_ori = "SEA")$ori)
})

test_that("agency labels keep the agency name and add the state", {
  expect_equal(rows$agency_label[rows$ori == "SEA"], "SEA Police Department (WA)")
  expect_setequal(rows$agency_label[rows$ori %in% c("GLN", "GLC")],
                  c("Glendale Police Department (AZ)", "Glendale Police Department (CA)"))
})

test_that("repeated agency labels stop", {
  twins <- bind_rows(city_agency("A", name = "Same"), city_agency("B", name = "Same"))
  expect_error(city_solve_rows(twins, map(c("A", "B"), city_offenses) |> list_rbind()), "share the label")
})

test_that("long data has a total row and one row per offense for each agency", {
  expect_equal(nrow(long), nrow(rows) * 5)
  expect_equal(levels(long$offense), c(total_label, violent_offenses))
  sea_homicide <- filter(long, ori == "SEA", offense == "Homicide")
  expect_false(sea_homicide$shown)
  expect_true(filter(long, ori == "SEA", offense == "Rape")$shown)
  expect_equal(filter(long, ori == "NYC", offense == total_label)$solved_rate, 0.69)
})

test_that("chart rows sort by solve rate and compare with the U.S. rate", {
  chart_rows <- agency_chart_rows(long, total_label, 0.46)
  expect_equal(chart_rows$ori, c("NYC", "GLC", "GLN", "SEA"))
  expect_equal(chart_rows$side, c("above", "above", "below", "below"))
  expect_equal(agencies_below(long, total_label, 0.46), 2)
  expect_equal(nrow(agency_chart_rows(long, "Homicide", 0.7)), 3)
  expect_match(chart_rows$tooltip[1], "69 of 100 incidents solved")
})

test_that("left-out notes count agencies with few incidents", {
  expect_equal(agency_left_out_note(long, total_label), "")
  expect_equal(agency_left_out_note(long, "Homicide"), " Leaves out 1 agency with fewer than 10 incidents.")
})

test_that("range text names the lowest and highest agencies", {
  expect_equal(
    agency_range_text(long, total_label),
    "33 percent (SEA Police Department, WA) to 69 percent (NYC Police Department, NY)"
  )
  expect_error(agency_range_text(filter(long, offense != "Rape"), "Rape"), "No agency")
})

test_that("agency charts plot one point per shown agency", {
  n_points <- function(chart) sum(map_int(chart$x$hc_opts$series, \(s) length(s$data)))
  bars <- agency_solve_bar_chart(long, "Homicide", 0.7, "Title", 2025)
  expect_equal(n_points(bars), 3)
  expect_false("SEA Police Department (WA)" %in% bars$x$hc_opts$xAxis$categories)
  expect_match(bars$x$hc_opts$subtitle$text, "Leaves out 1 agency")
  scatter <- agency_solve_scatter(long, total_label, 0.46, 400, "Title", 2025)
  expect_equal(n_points(scatter), 4)
  expect_equal(map_chr(scatter$x$hc_opts$series, "name"), unname(agency_side_names))
})

benchmarks <- tibble(
  state_abbr = rep(c("WA", "NY", "AZ", "CA"), each = 5),
  offense = rep(c(total_label, violent_offenses), 4),
  state_solve_rate = c(0.4, 0.6, 0.2, 0.3, 0.35, rep(0.5, 5), rep(NA, 5), rep(0.45, 5)),
  state_crime_rate = 300,
  state_solve_change = c(-2, rep(NA, 4), 4, rep(NA, 4), NA, rep(NA, 4), 1, rep(NA, 4))
)

test_that("state benchmarks join by state and offense", {
  with_states <- city_solve_long(rows, benchmarks = benchmarks)
  expect_equal(nrow(with_states), nrow(long))
  expect_equal(filter(with_states, ori == "SEA", offense == total_label)$state_solve_rate, 0.4)
  expect_false("state_solve_change" %in% names(with_states))
  chart_rows <- agency_chart_rows(with_states, total_label, 0.46)
  expect_match(chart_rows$tooltip[chart_rows$ori == "SEA"], "Solve rate: 33% \\(WA 40%\\)")
  expect_match(chart_rows$tooltip[chart_rows$ori == "GLN"], "Solve rate: 40%<br>", fixed = TRUE)
})

test_that("bar charts add a diamond for each agency with a state rate", {
  bars <- agency_solve_bar_chart(city_solve_long(rows, benchmarks = benchmarks), total_label, 0.46, "Title", 2025)
  markers <- keep(bars$x$hc_opts$series, \(s) s$name == state_marker_name)[[1]]
  expect_equal(markers$type, "scatter")
  expect_equal(length(markers$data), 3)
  expect_match(markers$data[[1]]$tooltip, "State solve rate")
})

test_that("change chart marks state changes where the state has one", {
  with_change <- left_join(rows, filter(benchmarks, offense == total_label) |> select(state_abbr, state_solve_change),
                           by = "state_abbr")
  changes <- agency_change_rows(with_change, 2019, 2025)
  expect_match(changes$tooltip[changes$ori == "SEA"], "\\+5 points \\(WA -2\\)")
  chart <- agency_solve_change_chart(with_change, 2, "Title", 2019, 2025)
  markers <- keep(chart$x$hc_opts$series, \(s) s$name == state_marker_name)[[1]]
  expect_equal(length(markers$data), 3)
})

test_that("change chart keeps agencies with a change and enough incidents", {
  changes <- agency_change_rows(rows, 2019, 2025)
  expect_equal(nrow(changes), 4)
  expect_true(all(changes$side == "above"))
  expect_match(changes$tooltip[changes$ori == "SEA"], "Solve rate \\(2019\\): 28%")
  chart <- agency_solve_change_chart(rows, 2, "Title", 2019, 2025)
  expect_equal(map_chr(chart$x$hc_opts$series, "name"), c(unname(agency_change_names), state_marker_name))
})
