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

test_that("solve rate column colors compare against its rate and label it", {
  definition <- solve_rate_column("Homicide", 0.5, "U.S. Rate")
  expect_equal(definition$name, "Homicide Solve Rate<br><br>(U.S. Rate: 50%)")
  expect_equal(definition$style(0.6)$color, "#15607A")
  expect_equal(definition$style(0.5)$color, "#15607A")
  expect_equal(definition$style(0.4)$color, "#E17619")
  expect_equal(definition$style(NA_real_)$color, "black")
})

test_that("solve rate columns map each offense to its rate and layout", {
  layout <- list(
    homicide = list(offense = "Homicide", align = "center"),
    rape = list(offense = "Rape", min_width = 155)
  )
  columns <- solve_rate_columns(c(rape = 0.3, homicide = 0.68), "NY State Rate", layout)
  expect_named(columns, c("solved_rate_homicide", "solved_rate_rape"))
  expect_equal(columns$solved_rate_homicide$name, "Homicide Solve Rate<br><br>(NY State Rate: 68%)")
  expect_equal(columns$solved_rate_homicide$align, "center")
  expect_equal(columns$solved_rate_rape$minWidth, 155)
  expect_equal(columns$solved_rate_rape$style(0.2)$color, "#E17619")
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

shr_weapons <- function() {
  tibble(
    geo_abbr = rep(c("US", "AA", "BB"), each = 6), group_cat = "weapon", year = 2023,
    group = rep(rep(c("Gun", "Knife", "Other"), each = 2), 3),
    indicator = rep(c("Incidents reported", "Incidents cleared"), 9),
    n = rep(c(10, 5, 4, 3, 2, 1), 3)
  )
}

test_that("SHR panel filters excluded groups and keeps requested order", {
  spec <- list(category = "weapon", title = "Rates", note = "Note",
               order = c("Knife", "Gun"), categories = c("Knife", "Gun"), exclude = "Other")
  prep <- function(category) function_shr_grouping_for_national_plot(shr_weapons(), category, 2022)
  chart <- shr_panel_chart(spec, prep, "2022-2024")
  expect_equal(chart$x$hc_opts$xAxis$categories, c("Knife", "Gun"))
  expect_equal(length(chart$x$hc_opts$series[[1]]$data), 2)
  expect_equal(chart$x$hc_opts$caption$text,
               "Note<br>FBI UCR Program, Supplementary Homicide Reports (2022-2024)")
  expect_false(chart$x$hc_opts$legend$enabled)
})

test_that("state SHR panel splits state and region into series with a legend", {
  prep <- function(category) {
    function_shr_grouping_for_state_plot(shr_weapons(), category, "AA", "BB", 2022, "Region", "State")
  }
  chart <- shr_panel_chart(shr_panels$weapon, prep, "2022-2024", series = "group_for_plot")
  series <- chart$x$hc_opts$series
  expect_equal(vapply(series, `[[`, character(1), "name"), c("State", "Region"))
  expect_equal(length(series[[1]]$data), 2)
  expect_true(chart$x$hc_opts$legend$enabled)
  expect_match(series[[1]]$accessibility$point$valueDescriptionFormat, "series.name")
})

test_that("compare trend draws the state solid and the US dashed in gray", {
  data <- tibble(
    year = rep(c(2023, 2024), 2),
    state_name = rep(c("United States", "State A"), each = 2),
    rate = c(10, 12, 20, 30), tooltip = "t"
  )
  chart <- trend_chart(data, "rate", "Title", "Sub", "Source", "rate",
                       color = "#123456", compare = TRUE)
  series <- chart$x$hc_opts$series
  expect_equal(vapply(series, `[[`, character(1), "name"), c("State A", "United States"))
  expect_equal(vapply(series, `[[`, character(1), "color"), c("#123456", jr_pal[7]))
  expect_equal(vapply(series, `[[`, character(1), "dashStyle"), c("solid", "dash"))
  expect_equal(vapply(series, `[[`, numeric(1), "opacity"), c(1, 0.75))
  expect_equal(chart$x$hc_opts$yAxis$min, 0)
  expect_equal(chart$x$hc_opts$yAxis$max, 40)
  expect_null(chart$x$hc_opts$legend$enabled)
})

test_that("trend y format replaces the default axis label format", {
  data <- tibble(year = c(2023, 2024), rate = c(40, 60), tooltip = "t")
  chart <- trend_chart(data, "rate", "Title", caption = "Source", value_label = "solve rate",
                       ceiling = 65, y_format = "{value}%")
  expect_equal(chart$x$hc_opts$yAxis$labels$format, "{value}%")
  expect_equal(chart$x$hc_opts$yAxis$max, 65)
  expect_null(chart$x$hc_opts$subtitle)
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
  expect_equal(chart$x$hc_opts$series[[1]]$data[[1]]$tooltip, "a")
  expect_equal(chart$x$hc_opts$colorAxis$max, 4)
  expect_true(chart$x$hc_opts$series[[1]]$nullInteraction)
  expect_true(chart$x$hc_opts$plotOptions$series$accessibility$enabled)
  expect_true(chart$x$hc_opts$plotOptions$series$accessibility$keyboardNavigation$enabled)
})

test_that("state map shows the tooltip column its spec names", {
  data <- tibble(state_abbr = "AA", rate = 5, url = "a",
                 tooltip_rate = "rate tip", tooltip_change = "change tip")
  spec <- list(value = "rate", tooltip = "tooltip_change", title = "Rate", subtitle = "",
               caption = "", colors = c("white", "blue"), label_format = "{value}",
               legend_width = 250, accessibility = "")
  point <- state_map_chart(data, list(type = "FeatureCollection", features = list()), spec)$x$hc_opts$series[[1]]$data[[1]]
  expect_equal(point$tooltip, "change tip")
  expect_false(any(startsWith(names(point), "tooltip_")))
})

test_that("metric trend uses the selected column and requested padding", {
  data <- tibble(year = c(2023, 2024), spending = c(10, 20),
                 staffing = c(2, 3), tooltip = c("a", "b"))
  chart <- metric_trend_chart(data, "staffing", "Staffing", "Per capita", "Source",
                              "officers per 10,000 residents", expansion = 2.5)
  expect_equal(chart$x$hc_opts$yAxis$min, 0)
  expect_equal(chart$x$hc_opts$yAxis$max, 5.5)
  expect_equal(vapply(chart$x$hc_opts$series[[1]]$data, `[[`, numeric(1), "y"), c(2, 3))
})
