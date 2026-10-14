library(testthat)
library(highcharter)
library(reactable)
library(tidyverse)

source("../R/utils.R")
source("../R/page-data.R")

test_that("state SHR panels pool recent incidents and compare with other regional states", {
  data <- tibble(
    year = rep(c(2021, 2022, 2025), each = 8),
    geo_abbr = rep(rep(c("AA", "BB"), each = 4), 3),
    group_cat = "victim_sex",
    group = rep(c("Female", "Female", "Unknown", "Unknown"), 6),
    indicator = rep(c("Incidents reported", "Incidents cleared"), 12),
    n = c(100, 0, 10, 0, 100, 0, 10, 0,
          10, 5, 10, 0, 20, 5, 10, 0,
          30, 15, 10, 0, 20, 15, 10, 0)
  )

  actual <- function_shr_grouping_for_state_plot(
    data, "victim_sex", "AA", "BB", 2022
  )

  expect_equal(as.character(actual$group_for_plot), c("AA", "Other region states"))
  expect_equal(actual$group, c("Female", "Female"))
  expect_equal(actual$clearance_rate, c(50, 50))
  expect_false(any(actual$group == "Unknown"))
})

test_that("state SHR panels omit groups with no reported incidents", {
  data <- tibble(
    year = c(2025, 2025), geo_abbr = "AA", group_cat = "weapon",
    group = "Zero", indicator = c("Incidents reported", "Incidents cleared"),
    n = c(0, 0)
  )

  actual <- function_shr_grouping_for_state_plot(data, "weapon", "AA", "BB", 2022)
  expect_equal(nrow(actual), 0L)
})

test_that("state SHR groups with few homicides lose their bar", {
  data <- tibble(
    year = 2022, geo_abbr = rep(c("AA", "AA", "BB", "BB"), 2), group_cat = "victim_race_eth",
    group = rep(c("Small", "Large"), each = 4),
    indicator = rep(c("Incidents reported", "Incidents cleared"), 4),
    n = c(9, 3, 50, 25, 40, 20, 50, 30)
  )

  actual <- function_shr_grouping_for_state_plot(data, "victim_race_eth", "AA", "BB", 2022)
  expect_true(is.na(filter(actual, group_for_plot == "AA", group == "Small")$clearance_rate))
  expect_equal(filter(actual, group_for_plot == "AA", group == "Large")$clearance_rate, 50)
  expect_match(filter(actual, group_for_plot == "AA", group == "Large")$tooltip, "Homicides solved: 20 of 40", fixed = TRUE)
})
