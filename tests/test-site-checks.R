library(testthat)

source("../R/site-checks.R")

write_page <- function(body) {
  path <- withr::local_tempfile(fileext = ".html", .local_envir = parent.frame())
  writeLines(paste0("<html><body>", body, "</body></html>"), path)
  path
}

test_that("bad values in visible text are reported with context", {
  page <- write_page(
    "<p>The rate rose NA percent since 2014.</p><li>Solve rate was NaN and -Inf.</li>"
  )
  found <- find_bad_values(page, context = 10)
  expect_equal(found$value, c("NA", "NaN", "-Inf"))
  expect_equal(found$context[1], "rate rose NA percent s")
  expect_equal(unique(found$file), basename(page))
})

test_that("alt, title and aria-label attributes are checked", {
  page <- write_page(paste0(
    "<img alt=\"Solve rate NA\">",
    "<a title=\"Change of Inf\">link</a>",
    "<div aria-label=\"Rate NaN\"></div>",
    "<span class=\"NA\">ok</span>"
  ))
  found <- find_bad_values(page)
  expect_setequal(found$value, c("NA", "Inf", "NaN"))
})

test_that("widget data and ordinary words are not flagged", {
  page <- write_page(paste0(
    "<p>More Information on NAICS codes in Nashville.</p>",
    "<script>var x = {\"y\": NA, \"z\": Inf};</script>",
    "<style>.NA { color: red; }</style>"
  ))
  found <- find_bad_values(page)
  expect_equal(nrow(found), 0)
  expect_named(found, c("file", "value", "context"))
})
