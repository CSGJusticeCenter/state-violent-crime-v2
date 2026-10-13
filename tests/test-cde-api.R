library(testthat)

source("../R/cde-api.R")

### fake curl responses, counting calls
mock_fetch <- function(status, calls) {
  function(url) {
    calls$n <- calls$n + 1
    list(status_code = status, content = charToRaw('{"offenses": 1}'))
  }
}

test_that("a successful call returns the parsed json", {
  withr::local_envvar(DATA_GOV_API_KEY = "secretkey")
  calls <- new.env()
  calls$n <- 0
  local_mocked_bindings(curl_fetch_memory = mock_fetch(200L, calls), .package = "curl")
  expect_equal(read_cde_summarized("national", "robbery", 2022), list(offenses = 1))
  expect_equal(calls$n, 1)
})

test_that("client errors stop without retrying", {
  withr::local_envvar(DATA_GOV_API_KEY = "secretkey")
  calls <- new.env()
  calls$n <- 0
  local_mocked_bindings(curl_fetch_memory = mock_fetch(403L, calls), .package = "curl")
  expect_error(read_cde_summarized("national", "robbery", 2022), "HTTP status 403")
  expect_equal(calls$n, 1)
})

test_that("outages retry until tries run out", {
  withr::local_envvar(DATA_GOV_API_KEY = "secretkey")
  calls <- new.env()
  calls$n <- 0
  local_mocked_bindings(curl_fetch_memory = mock_fetch(503L, calls), .package = "curl")
  expect_error(read_cde_summarized("national", "robbery", 2022, tries = 2), "HTTP status 503")
  expect_equal(calls$n, 2)
})

test_that("connection errors leave the key out of the message", {
  withr::local_envvar(DATA_GOV_API_KEY = "secretkey")
  local_mocked_bindings(curl_fetch_memory = function(url) stop("failed to open ", url), .package = "curl")
  err <- expect_error(read_cde_summarized("national", "robbery", 2022, tries = 1))
  expect_match(conditionMessage(err), "<key>", fixed = TRUE)
  expect_no_match(conditionMessage(err), "secretkey", fixed = TRUE)
})
