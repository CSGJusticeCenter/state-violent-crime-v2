library(testthat)
library(highcharter)
library(reactable)
library(tidyverse)

source("../R/utils.R")

test_that("display formatters round halves up", {
  expect_equal(comma(c(2.5, 3.5, 1234.5)), c("3", "4", "1,235"))
  expect_equal(comma(0.25, 0.1), "0.3")
  expect_equal(comma(-2.5), "-3")
  expect_equal(comma(0.0036, scale = 1e5), "360")
  expect_equal(percent(c(0.625, 0.125, 0.355)), c("63%", "13%", "36%"))
  expect_equal(percent(0.475, suffix = " percent"), "48 percent")
  expect_equal(dollar(2.5), "$3")
  expect_equal(dollar(1.25e9, accuracy = 0.1, scale = 1e-9, suffix = "B"), "$1.3B")
  expect_identical(comma(NA_real_), scales::comma(NA_real_))
})

test_that("rates per 100k show one decimal only for homicide", {
  expect_equal(format_rate_100k(c(4.5, 62.2, 1234.5, NA)), c("5", "62", "1,235", ""))
  expect_equal(format_rate_100k(c(4.05, 0, 0.04, NA), homicide = TRUE), c("4.1", "0", "0", ""))
  expect_equal(format_rate_100k(c(4.05, 62.25), c(TRUE, FALSE)), c("4.1", "62"))
})

test_that("rate cell functions ignore the row index reactable passes", {
  expect_equal(rate_100k_cell(348.3, 2), "348")
  expect_equal(homicide_rate_100k_cell(8.53, 2), "8.5")
  expect_equal(homicide_rate_100k_cell(0, 1), "0")
})

test_that("signed cells show a sign, round, and leave missing values blank", {
  expect_equal(add_plus_sign_percent_point_change(3.4), "+3")
  expect_equal(add_plus_sign_percent_point_change(-5.6), "-6")
  expect_equal(add_plus_sign_percent_point_change(0.3), "0")
  expect_equal(add_plus_sign_percent_point_change(-0.3), "0")
  expect_equal(add_plus_sign_percent_point_change(NA_real_), na_text)
  expect_equal(add_plus_sign_percent_point_change(2.5), "+3")
  expect_equal(add_plus_sign_percent_change(12), "+12%")
  expect_equal(add_plus_sign_percent_change(NA_real_), na_text)
})

test_that("change phrases describe direction, size and units", {
  expect_equal(change_phrase(-0.04), "4 percent lower than")
  expect_equal(change_phrase(0.12), "12 percent higher than")
  expect_equal(change_phrase(0.004), "nearly the same as")
  expect_equal(change_phrase(0.01, "point"), "1 percentage point higher than")
  expect_equal(change_phrase(-0.03, "point"), "3 percentage points lower than")
})

test_that("change phrases switch wording at the band edges", {
  expect_equal(change_phrase(0.005), "nearly the same as")
  expect_equal(change_phrase(-0.006, "point"), "1 percentage point lower than")
  expect_equal(change_phrase(0.0149, "point"), "1 percentage point higher than")
  expect_equal(change_phrase(0.015, "point"), "2 percentage points higher than")
})

test_that("change direction compares numbers, not formatted text", {
  expect_equal(change_direction(-0.05), "decreased")
  expect_equal(change_direction(0.12), "increased")
  expect_equal(change_direction(-0.05, "an increase", "a decrease"), "a decrease")
})

test_that("change by gives the size, or about the same near zero", {
  expect_equal(change_by(0.18), "increased by 18 percent")
  expect_equal(change_by(-0.034), "decreased by 3 percent")
  expect_equal(change_by(-0.004), "stayed about the same")
})

test_that("text helpers stop on missing or empty input", {
  expect_error(change_phrase(NA_real_), "one finite change value")
  expect_error(change_direction(numeric(0)), "one finite change value")
  expect_error(change_by(Inf), "one finite change value")
})

test_that("percent change runs between two named years", {
  expect_equal(pct_change_between(c(120, 100, 150), c(2025, 2015, 2020), 2015, 2025), 0.2)
  expect_equal(pct_change_between(c(120, 100, 150), c(2025, 2015, 2020), 2020, 2025), -0.2)
})

test_that("percent change stops when a year is missing or repeated", {
  expect_error(pct_change_between(c(5, 6), c(2024, 2025), 2019, 2025), "one 2019 value")
  expect_error(pct_change_between(c(5, 6, 7), c(2019, 2019, 2025), 2019, 2025), "found 2")
})
