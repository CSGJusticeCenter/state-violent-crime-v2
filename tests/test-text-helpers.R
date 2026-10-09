library(testthat)
library(highcharter)
library(reactable)
library(tidyverse)

source("../R/utils.R")

test_that("signed cells show a sign, round, and leave missing values blank", {
  expect_equal(add_plus_sign_percent_point_change(3.4), "+3")
  expect_equal(add_plus_sign_percent_point_change(-5.6), "-6")
  expect_equal(add_plus_sign_percent_point_change(0.3), "0")
  expect_equal(add_plus_sign_percent_point_change(-0.3), "0")
  expect_equal(add_plus_sign_percent_point_change(NA_real_), "")
  expect_equal(add_plus_sign_percent_change(12), "+12%")
  expect_equal(add_plus_sign_percent_change(NA_real_), "")
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

test_that("percent change runs from the first to the last year", {
  expect_equal(pct_change_first_last(c(120, 100, 150), c(2025, 2015, 2020)), 0.2)
})
