library(testthat)
library(highcharter)
library(reactable)
library(tidyverse)

source("../R/page-data.R")
source("../R/utils.R")

rape <- tribble(
  ~agency_name, ~state_abbr, ~population, ~n_incidents, ~n_arrest, ~n_exceptional, ~n_not_cleared,
  ~n_exceptional_prosecution_declined, ~n_exceptional_victim_refused_to_cooperate,
  "Mostly exceptional", "FL", 500000, 100, 20, 60, 20, 50, 10,
  "Mostly arrests", "FL", 400000, 50, 30, 5, 15, 0, 5,
  "Rarely cleared", "FL", 300000, 40, 2, 2, 36, 2, 0,
  "Few incidents", "FL", 600000, 9, 9, 0, 0, 0, 0
) |>
  mutate(offense = "Rape")
total <- tribble(
  ~agency_name, ~state_abbr, ~population, ~n_incidents, ~n_arrest, ~n_exceptional, ~n_not_cleared,
  ~n_exceptional_prosecution_declined, ~n_exceptional_victim_refused_to_cooperate,
  "Mostly exceptional", "FL", 500000, 1000, 500, 200, 300, 150, 50,
  "Mostly arrests", "FL", 400000, 500, 400, 20, 80, 10, 10
) |>
  mutate(offense = total_label)
clearance <- bind_rows(rape, total)

test_that("incidents take their most serious violent offense", {
  expect_equal(nibrs_top_offense(c("13A", "120")), "Robbery")
  expect_equal(nibrs_top_offense(c("11A", "09A", "13A")), "Homicide")
  expect_equal(nibrs_top_offense(c("11C", "120")), "Rape")
  expect_equal(nibrs_top_offense("13A"), "Aggravated assault")
  expect_true(is.na(nibrs_top_offense(c("09B", "11D", "23H"))))
})

test_that("rows keep one offense, drop agencies with few incidents and shares sum to 100", {
  rows <- clearance_rows(clearance, "Rape", "cleared")
  expect_false("Few incidents" %in% rows$agency_name)
  expect_equal(rows$pct_arrest + rows$pct_exceptional + rows$pct_not_cleared, rep(100, 3))
  expect_setequal(clearance_rows(clearance, total_label, "cleared")$n_incidents, c(1000, 500))
})

test_that("rows sort by the chosen outcome", {
  expect_equal(clearance_rows(clearance, "Rape", "cleared")$agency_name,
               c("Mostly exceptional", "Mostly arrests", "Rarely cleared"))
  expect_equal(clearance_rows(clearance, "Rape", "exceptional")$agency_name,
               c("Mostly exceptional", "Mostly arrests", "Rarely cleared"))
  expect_equal(clearance_rows(clearance, "Rape", "arrest")$agency_name,
               c("Mostly arrests", "Mostly exceptional", "Rarely cleared"))
  expect_equal(clearance_rows(clearance, total_label, "cleared")$agency_name,
               c("Mostly arrests", "Mostly exceptional"))
  expect_error(clearance_rows(clearance, "Rape", "unsolved"))
})

test_that("tooltips name the offense and list exceptional reasons, largest first", {
  tip <- clearance_rows(clearance, "Rape", "cleared")$tooltip[1]
  expect_match(tip, "100 rape incidents", fixed = TRUE)
  expect_match(tip, "Solved by exceptional means: 60% (60)<br>&nbsp;&nbsp;Prosecution declined: 50<br>&nbsp;&nbsp;Victim refused to cooperate: 10",
               fixed = TRUE)
  expect_false(grepl("Prosecution declined", clearance_rows(clearance, "Rape", "arrest")$tooltip[1]))
  expect_match(clearance_rows(clearance, total_label, "cleared")$tooltip[1], "500 violent crime incidents", fixed = TRUE)
})

test_that("the chart stacks the three outcomes in order with site colors", {
  chart <- clearance_chart(clearance, "Rape", "arrest", "Title", 2025)
  series <- chart$x$hc_opts$series
  expect_equal(map_chr(series, "name"), unname(clearance_outcome_names))
  expect_equal(map_chr(series, "color"), unname(clearance_outcome_colors))
  expect_equal(chart$x$hc_opts$xAxis$categories, c("Mostly arrests (FL)", "Mostly exceptional (FL)", "Rarely cleared (FL)"))
  expect_equal(map_dbl(series[[1]]$data, "y"), c(60, 20, 5))
  expect_equal(chart$x$hc_opts$plotOptions$bar$stacking, "normal")
  expect_match(chart$x$hc_opts$subtitle$text, "^Percentage of rape incidents")
  expect_match(clearance_chart(clearance, total_label, "cleared", "Title", 2025)$x$hc_opts$subtitle$text,
               "^Percentage of violent crime incidents")
})

test_that("color words use the chart colors, with light blue as a highlight", {
  expect_match(clearance_outcome_color_text("exceptional", "orange"), clearance_outcome_colors[["exceptional"]], fixed = TRUE)
  light <- clearance_outcome_color_text("not_cleared", "light blue")
  expect_match(light, paste0("background-color:", clearance_outcome_colors[["not_cleared"]]), fixed = TRUE)
  expect_match(light, ">light blue</span>$")
})
