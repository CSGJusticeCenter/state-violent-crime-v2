library(testthat)
library(highcharter)
library(reactable)
library(tidyverse)

source("../R/utils.R")

test_that("chart bounds expand the data and respect a zero floor", {
  expect_equal(chart_bounds(c(10, 20), expansion = 0.5), c(5, 25))
  expect_equal(chart_bounds(c(0, 2), expansion = 0.5, floor = 0), c(0, 3))
  expect_equal(chart_bounds(c(98, 100), expansion = 0.5, ceiling = 100), c(97, 100))
})

test_that("change maps center their scale on zero", {
  expect_equal(change_map_bounds(c(-4, 2)), c(-4, 4))
  expect_equal(change_map_bounds(c(NA, -2, 5)), c(-5, 5))
})

test_that("solve rate column colors compare against its own median", {
  definition <- solve_rate_column("Homicide", 0.5)
  expect_equal(definition$style(0.6)$color, "#15607A")
  expect_equal(definition$style(0.4)$color, "#E17619")
  expect_equal(definition$style(NA_real_)$color, "black")
})

test_that("offense trend uses the selected offense and its own rate range", {
  data <- tibble(
    year = c(2023, 2024, 2023, 2024),
    group = c("Homicide", "Homicide", "Robbery", "Robbery"),
    incidents_reported_rate_total = c(5, 7, 40, 50),
    tooltip = c("h1", "h2", "r1", "r2")
  )
  chart <- offense_trend_chart(data, "Homicide", "Homicide", "Rate", "#123456")
  expect_equal(chart$x$hc_opts$yAxis$min, 4)
  expect_equal(chart$x$hc_opts$yAxis$max, 8)
  expect_equal(chart$x$hc_opts$title$text, "Homicide")
  expect_equal(length(chart$x$hc_opts$series[[1]]$data), 2)
})

test_that("SHR chart keeps a zero to 100 percent axis", {
  data <- tibble(group = c("A", "B"), clearance_rate = c(25, 75), tooltip = c("a", "b"))
  chart <- shr_rate_chart(data, "Rates", "2022-2024", "Source")
  expect_equal(chart$x$hc_opts$yAxis$min, 0)
  expect_equal(chart$x$hc_opts$yAxis$max, 100)
  expect_equal(chart$x$hc_opts$title$text, "Rates")
})

test_that("SHR panel filters excluded groups and keeps requested order", {
  data <- tibble(
    geo_abbr = "US", group_cat = "weapon", year = 2023,
    group = rep(c("Gun", "Knife", "Other"), each = 2),
    indicator = rep(c("Incidents reported", "Incidents cleared"), 3),
    n = c(10, 5, 4, 3, 2, 1)
  )
  spec <- list(category = "weapon", title = "Rates", caption = "Source",
               order = c("Knife", "Gun"), categories = c("Knife", "Gun"), exclude = "Other")
  chart <- shr_panel_chart(data, spec, 2022, "2022-2024")
  expect_equal(chart$x$hc_opts$xAxis$categories, c("Knife", "Gun"))
  expect_equal(length(chart$x$hc_opts$series[[1]]$data), 2)
})

test_that("state change map uses a symmetric scale and preserves null interaction", {
  data <- tibble(state_abbr = c("AA", "BB"), change = c(-4, 2),
                 tooltip = c("a", "b"), url = c("a", "b"))
  spec <- list(value = "change", title = "Change", subtitle = "Rate change",
               caption = "Source", colors = c("blue", "white", "orange"),
               label_format = "{value}%", legend_width = 250,
               accessibility = "state: {point.state_abb}, change: {point.value:.1f}",
               symmetric = TRUE, null_interaction = TRUE)
  chart <- state_map_chart(data, list(type = "FeatureCollection", features = list()), spec)
  expect_equal(chart$x$hc_opts$colorAxis$min, -4)
  expect_equal(chart$x$hc_opts$colorAxis$max, 4)
  expect_true(chart$x$hc_opts$series[[1]]$nullInteraction)
  expect_true(chart$x$hc_opts$plotOptions$series$accessibility$enabled)
  expect_true(chart$x$hc_opts$plotOptions$series$accessibility$keyboardNavigation$enabled)
})

test_that("metric trend uses the selected column and requested padding", {
  data <- tibble(year = c(2023, 2024), spending = c(10, 20),
                 staffing = c(2, 3), tooltip = c("a", "b"))
  chart <- metric_trend_chart(data, "staffing", "Staffing", "Per capita", "Source",
                              "year: {point.x:.0f}, rate: {point.y:.1f}", expansion = 2.5)
  expect_equal(chart$x$hc_opts$yAxis$min, 0)
  expect_equal(chart$x$hc_opts$yAxis$max, 5.5)
  expect_equal(vapply(chart$x$hc_opts$series[[1]]$data, `[[`, numeric(1), "y"), c(2, 3))
})
