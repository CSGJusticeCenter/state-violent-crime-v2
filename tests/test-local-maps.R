library(testthat)

source("../R/local-map-data.R")

test_that("county map loads from a local Highcharts JS file", {
  root <- tempfile("maps-")
  dir.create(file.path(root, "countries", "us"), recursive = TRUE)
  path <- file.path(root, "countries", "us", "us-ny-all.js")
  writeLines(
    'Highcharts.maps["countries/us/us-ny-all"] = {"type":"FeatureCollection","features":[{"type":"Feature","properties":{"fips":"36001"}}]};',
    path
  )

  map <- read_local_county_map("countries/us/us-ny-all.js", root)

  expect_equal(map$type, "FeatureCollection")
  expect_equal(map$features[[1]]$properties$fips, "36001")
})

test_that("missing county map reports which file to restore", {
  root <- tempfile("maps-")
  expect_error(
    read_local_county_map("countries/us/us-ny-all.js", root),
    "us-ny-all.js"
  )
})

test_that("all state pages have offline county maps with FIPS join keys", {
  for (state in tolower(c(state.abb, "DC"))) {
    map <- read_local_county_map(sprintf("countries/us/us-%s-all.js", state), "../maps")
    expect_equal(map$type, "FeatureCollection", info = state)
    expect_true(length(map$features) > 0L, info = state)
    expect_true(all(vapply(map$features, function(feature) {
      !is.null(feature$properties$fips)
    }, logical(1))), info = state)
  }
})

test_that("local county chart embeds shapes and maps values by FIPS", {
  data <- data.frame(county_fips = "36001", solved_rate = 42)
  map <- hcmap_local(
    "countries/us/us-ny-all",
    data = data,
    value = "solved_rate",
    joinBy = c("fips", "county_fips"),
    root = "../maps"
  )
  features <- map$x$hc_opts$series[[1]]$mapData$features
  expect_gt(length(features), 55L)
  expect_true(any(vapply(features, function(feature) {
    identical(feature$properties$fips, "36001")
  }, logical(1))))
  expect_equal(map$x$hc_opts$series[[1]]$joinBy, c("fips", "county_fips"))
  expect_equal(map$x$hc_opts$series[[1]]$data[[1]]$value, 42)
})

test_that("NYC boroughs load as one feature", {
  map <- read_local_county_map("countries/us/us-ny-all.js", "../maps")
  fips <- vapply(map$features, function(feature) feature$properties$fips, character(1))

  expect_false(any(nyc_borough_fips %in% fips))
  expect_equal(sum(fips == nyc_fips), 1L)
  expect_equal(length(fips), 58L)

  nyc <- map$features[[which(fips == nyc_fips)]]
  expect_equal(nyc$geometry$type, "MultiPolygon")
  expect_true(sf::st_is_valid(geometry_to_sfg(nyc$geometry)))
})

test_that("other states are not changed by the NYC merge", {
  map <- read_local_county_map("countries/us/us-nj-all.js", "../maps")
  fips <- vapply(map$features, function(feature) feature$properties$fips, character(1))
  expect_false(nyc_fips %in% fips)
})
