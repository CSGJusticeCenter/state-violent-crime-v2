library(testthat)
library(highcharter)
library(reactable)
library(tidyverse)

source("../R/page-data.R")
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

test_that("signed cells show a sign, round, and show missing values as a dash", {
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
  expect_equal(change_phrase(-0.04), "4% lower than")
  expect_equal(change_phrase(0.12), "12% higher than")
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
  expect_equal(change_by(0.18), "increased by 18%")
  expect_equal(change_by(-0.034), "decreased by 3%")
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

test_that("solve rates read as whole percents with a % sign", {
  expect_equal(solve_rate_text(0.475), "48%")
})

test_that("title_case capitalizes words but not small words or existing capitals", {
  expect_equal(
    title_case("Violent crime reported, solved, and unsolved in the U.S., 2020–2025"),
    "Violent Crime Reported, Solved, and Unsolved in the U.S., 2020–2025"
  )
  expect_equal(title_case("the 5-year average per 100k in DC"), "The 5-Year Average per 100k in DC")
  expect_equal(title_case("West region"), "West Region")
  expect_equal(title_case(c("", NA)), c("", NA))
})

test_that("title_case capitalizes small words that start a sentence, and words in parentheses", {
  expect_equal(
    title_case("Percentage solved, 2025. the dashed line marks the U.S. rate"),
    "Percentage Solved, 2025. The Dashed Line Marks the U.S. Rate"
  )
  expect_equal(title_case("rate in the U.S. by offense"), "Rate in the U.S. by Offense")
  expect_equal(title_case("violent crimes (log scale)"), "Violent Crimes (Log Scale)")
})
