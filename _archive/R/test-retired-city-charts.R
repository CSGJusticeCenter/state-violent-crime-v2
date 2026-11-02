# Retired tests for the funnel plot and offense panels in _archive/R/utils-retired.R.
# They used the fixtures in tests/test-city-solve.R; nothing runs this file.

test_that("funnel limits narrow as incidents grow and stop at zero", {
  wide <- funnel_limits(0.5, 10, funnel_z[["outer"]])
  narrow <- funnel_limits(0.5, 1000, funnel_z[["outer"]])
  expect_gt(wide$high - wide$low, narrow$high - narrow$low)
  expect_equal(funnel_limits(0.05, 4, 3)$low, 0)
  expect_equal(funnel_limits(0.5, 100, 1.96)$high, 0.5 + 1.96 * 0.05)
})

test_that("funnel rows flag rates beyond the outer limits", {
  sides <- funnel_rows(long, total_label, 0.46)$funnel_side
  expect_equal(sides, c("above", "within", "within", "within"))
  expect_equal(funnel_rows(long, total_label, 0.8)$funnel_side, c("within", rep("below", 3)))
  expect_equal(funnel_counts(long, total_label, 0.46)[["n"]], 4)
  chart <- agency_funnel_chart(long, total_label, 0.46, "Title", 2025)
  expect_equal(chart$x$hc_opts$xAxis$type, "logarithmic")
  expect_equal(chart$x$hc_opts$series[[1]]$type, "arearange")
})

test_that("offense panels share one agency order and one axis per offense", {
  us <- set_names(c(0.3, 0.7, 0.2, 0.3), table_offenses)
  chart <- agency_offense_panels(long, us, "Title", 2025)
  expect_length(chart$x$hc_opts$yAxis, 4)
  expect_equal(chart$x$hc_opts$xAxis$categories[1], "NYC Police Department (NY)")
  expect_equal(agencies_consistent(long, us)[["n"]], 3)
})

