library(testthat)
library(highcharter)
library(reactable)
library(tidyverse)

source("../R/utils.R")
source("../R/page-data.R")

test_that("chart bounds expand the data and respect a zero floor", {
  expect_equal(chart_bounds(c(10, 20), expansion = 0.5), c(5, 25))
  expect_equal(chart_bounds(c(0, 2), expansion = 0.5, floor = 0), c(0, 3))
  expect_equal(chart_bounds(c(98, 100), expansion = 0.5, ceiling = 100), c(97, 100))
})

test_that("offense colors come from the offense palette", {
  expect_equal(offense_color("Homicide"), jr_pal[2])
  expect_equal(offense_color("Aggravated assault"), jr_pal[5])
})

test_that("change maps center their scale on zero", {
  expect_equal(change_map_bounds(c(-4, 2)), c(-4, 4))
  expect_equal(change_map_bounds(c(NA, -2, 5)), c(-5, 5))
})

test_that("solve rate column tints pills against its rate and labels it", {
  definition <- solve_rate_column("Homicide", 0.5, "U.S. Rate")
  expect_equal(definition$name, "Homicide Solve Rate<br><br>(U.S. Rate: 50%)")
  expect_equal(definition$cell(0.6), solve_rate_pill("60%", "above", "at or above, "))
  expect_equal(definition$cell(0.5), solve_rate_pill("50%", "above", "at or above, "))
  expect_equal(definition$cell(0.4), solve_rate_pill("40%", "below", "below, "))
  expect_equal(definition$cell(NA_real_), "")
})

test_that("solve rate pill styles by side and hides screen reader words", {
  expect_equal(
    solve_rate_pill("40%", "below", "below, "),
    paste0(
      '<span class="solve-rate-pill solve-rate-pill-below" style="color:#9A4A0A;background-color:#FBE8DA">',
      '<span class="visually-hidden">below, </span>40%</span>'
    )
  )
  expect_no_match(solve_rate_pill("Blue", "above"), "visually-hidden")
})

test_that("above and below pills differ by more than color", {
  ### only above pills get the outline class, and only below pills a tinted fill
  expect_match(solve_rate_pill("60%", "above"), "solve-rate-pill-above", fixed = TRUE)
  expect_equal(unname(solve_rate_fills[["above"]]), "#FFFFFF")
  expect_false(solve_rate_fills[["below"]] == "#FFFFFF")

  css <- readLines("../styles.css")
  outline <- grep("^\\.solve-rate-pill-above", css)
  expect_length(outline, 1)
  expect_match(css[outline + 1], "border-color: currentColor", fixed = TRUE)
})

test_that("solve rates compare at the whole percents the table shows", {
  expect_equal(solve_rate_side(0.357, 0.362), "above")
  expect_equal(solve_rate_side(0.354, 0.356), "below")
  expect_equal(solve_rate_side(0.40, 0.36), "above")
  expect_equal(solve_rate_side(0.30, 0.36), "below")
  expect_true(is.na(solve_rate_side(NA_real_, 0.36)))
  expect_true(is.na(solve_rate_side(0.36, NA_real_)))

  definition <- solve_rate_column("Robbery", 0.362, "U.S. Rate")
  expect_equal(definition$name, "Robbery Solve Rate<br><br>(U.S. Rate: 36%)")
  expect_match(definition$cell(0.357), solve_rate_fills[["above"]], fixed = TRUE)
})

test_that("solve rate cells and headers round halves up the same way", {
  expect_equal(format_solve_rate(0.625), "63%")
  expect_equal(format_solve_rate(0.125), "13%")
  expect_equal(format_solve_rate(0.355), "36%")
  expect_equal(format_solve_rate(0.3549), "35%")
  expect_equal(format_solve_rate(1.25), "125%")
  expect_equal(format_solve_rate(NA_real_), "")

  # Evansville PD homicide, 5 of 8, against Indiana's 63 percent
  expect_equal(solve_rate_side(5 / 8, 0.633), "above")
  definition <- solve_rate_column("Homicide", 0.633, "IN State Rate")
  expect_equal(definition$cell(5 / 8), solve_rate_pill("63%", "above", "at or above, "))
  expect_equal(definition$name, "Homicide Solve Rate<br><br>(IN State Rate: 63%)")
})

test_that("solve rate column shows plain text when its rate is missing", {
  definition <- solve_rate_column("Rape", NA_real_, "NY State Rate")
  expect_equal(definition$cell(0.4), "40%")
})

test_that("solve rate legend shows the color name in the table's pill", {
  expect_equal(
    solve_rate_legend("below", "indicates a lower rate."),
    paste(solve_rate_pill("Orange", "below"), "indicates a lower rate.")
  )
  expect_match(solve_rate_legend("above", "x"), "Blue</span> x", fixed = TRUE)
})

test_that("solve rate text colors reach 4.5:1 contrast on their pill fills", {
  luminance <- function(hex) {
    v <- grDevices::col2rgb(hex)[, 1] / 255
    v <- ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * v)
  }
  contrast <- mapply(
    \(text, fill) (luminance(fill) + 0.05) / (luminance(text) + 0.05),
    solve_rate_colors, solve_rate_fills[names(solve_rate_colors)]
  )
  expect_true(all(contrast >= 4.5))
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
  expect_equal(columns$solved_rate_rape$cell(0.2), solve_rate_pill("20%", "below", "below, "))
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
    n = rep(c(100, 50, 40, 30, 20, 10), 3)
  )
}

test_that("SHR panel filters excluded groups and keeps requested order", {
  spec <- list(category = "weapon", title = "Rates", note = "Note",
               order = c("Knife", "Gun"), exclude = "Other")
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

test_that("sorted SHR panels share one pooled order across series", {
  ### the state has no knife cases; pooled, knife (90%) ranks above gun (50%)
  data <- tibble(
    group_for_plot = c("State", "Region", "Region"),
    group = c("Gun", "Gun", "Knife"),
    `Incidents reported` = c(10, 10, 10),
    `Incidents cleared` = c(5, 5, 9),
    clearance_rate = c(50, 50, 90),
    tooltip = "t"
  )
  prep <- function(category) data
  chart <- shr_panel_chart(shr_panels$weapon, prep, "2022-2024", series = "group_for_plot")
  expect_equal(chart$x$hc_opts$xAxis$categories, c("Knife", "Gun"))
  expect_equal(shr_category_order(data, list(sort_desc = TRUE)), c("Knife", "Gun"))
  expect_null(shr_category_order(data, list()))
})

test_that("SHR order lists only groups in the data", {
  data <- tibble(group = "Single victim")
  expect_equal(shr_category_order(data, shr_panels$victims), "Single victim")
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

test_that("single trend hides its legend unless asked", {
  data <- tibble(year = c(2023, 2024), rate = c(1, 2), tooltip = "t")
  expect_false(trend_chart(data, "rate", "T", caption = "S", value_label = "rate")$x$hc_opts$legend$enabled)
  expect_null(trend_chart(data, "rate", "T", caption = "S", value_label = "rate",
                          legend = TRUE)$x$hc_opts$legend$enabled)
})

test_that("trend y formatter sits beside the default label format", {
  data <- tibble(year = c(2023, 2024), dollars = c(1e9, 2e9), tooltip = "t")
  formatter <- "function() { return '$' + (this.value / 1000000000) + 'B'; }"
  chart <- trend_chart(data, "dollars", "T", caption = "S", value_label = "expenditure",
                       value_format = "${point.y:,.0f}", y_formatter = formatter)
  labels <- chart$x$hc_opts$yAxis$labels
  expect_equal(as.character(labels$formatter), formatter)
  expect_equal(labels$format, "{value:,.0f}")
  expect_equal(chart$x$hc_opts$series[[1]]$accessibility$point$valueDescriptionFormat,
               "year: {point.x:.0f}, expenditure: ${point.y:,.0f}")
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

test_that("trend uses the selected column and requested padding", {
  data <- tibble(year = c(2023, 2024), spending = c(10, 20),
                 staffing = c(2, 3), tooltip = c("a", "b"))
  chart <- trend_chart(data, "staffing", "Staffing", "Per capita", "Source",
                       "officers per 10,000 residents", expansion = 2.5)
  expect_equal(chart$x$hc_opts$yAxis$min, 0)
  expect_equal(chart$x$hc_opts$yAxis$max, 5.5)
  expect_equal(vapply(chart$x$hc_opts$series[[1]]$data, `[[`, numeric(1), "y"), c(2, 3))
})

test_that("hollow year markers apply only to points in that year", {
  df <- tibble(year = rep(c(2020, 2021, 2022), 2), y = 1:6, g = rep(c("a", "b"), each = 3))
  chart <- hchart(df, "spline", hcaes(year, y, group = g)) |>
    hc_colors(c("#111111", "#222222")) |>
    hc_hollow_year(2021)
  points_a <- chart$x$hc_opts$series[[1]]$data
  points_b <- chart$x$hc_opts$series[[2]]$data

  expect_null(points_a[[1]]$marker)
  expect_equal(points_a[[2]]$marker, list(fillColor = "#FFFFFF", lineColor = "#111111", lineWidth = 2))
  expect_equal(points_b[[2]]$marker$lineColor, "#222222")
  expect_null(points_a[[3]]$marker)
})

test_that("hollow year markers use a series' own color first", {
  df <- tibble(year = c(2020, 2021), y = 1:2)
  chart <- hchart(df, "spline", hcaes(year, y), color = "#333333") |> hc_hollow_year(2021)
  expect_equal(chart$x$hc_opts$series[[1]]$data[[2]]$marker$lineColor, "#333333")
})

test_that("hollow year markers stop without a series color", {
  df <- tibble(year = c(2020, 2021), y = 1:2)
  expect_error(hchart(df, "spline", hcaes(year, y)) |> hc_hollow_year(2021), "needs a color")
})

test_that("solve rates from few incidents show gray without a pill", {
  definition <- solve_rate_column("Homicide", 0.5, "NY State Rate",
                                  reported = c(9, 10), solved = c(9, 3))
  small <- definition$cell(1, 1)
  expect_no_match(small, "solve-rate-pill")
  expect_match(small, 'class="small-n-note"', fixed = TRUE)
  expect_match(small, "fewer than 10 incidents", fixed = TRUE)
  expect_match(small, 'title="9 of 9 incidents solved"', fixed = TRUE)
  expect_equal(
    definition$cell(0.3, 2),
    solve_rate_pill("30%", "below", "below, ", "3 of 10 incidents solved")
  )
})

test_that("solve rate columns take counts from the table data", {
  layout <- list(rape = list(offense = "Rape"))
  counts <- tibble(n_reported_rape = c(4, 40), n_solved_rape = c(1, 20))
  columns <- solve_rate_columns(c(rape = 0.3), "NY State Rate", layout, counts = counts)
  expect_match(columns$solved_rate_rape$cell(0.25, 1), "small-n-note", fixed = TRUE)
  expect_match(columns$solved_rate_rape$cell(0.5, 2), "20 of 40", fixed = TRUE)
})

test_that("small-n cells gray rows under the minimum and tell screen readers why", {
  cell <- small_n_cell(c(9, 10, NA), format_solve_rate, min_reported = 10)
  expect_match(cell(0.5, 1), 'class="small-n-note"', fixed = TRUE)
  expect_match(cell(0.5, 1), "visually-hidden\"> (fewer than 10 incidents)", fixed = TRUE)
  expect_equal(cell(0.5, 2), "50%")
  expect_equal(cell(0.5, 3), "50%")
  expect_equal(cell(NA, 1), "")
})

test_that("reactable cell functions see rows in data order, not display order", {
  rows <- tibble(agency = c("b", "a", "c"), pop = c(1, 3, 2), n = c(5, 50, 9))
  table <- reactable(
    rows, defaultSorted = list(pop = "desc"),
    columns = list(agency = colDef(cell = function(value, index) paste(value, rows$n[index])))
  )
  cells <- unlist(table$x$tag$attribs$columns[[1]]$cell)
  expect_equal(cells, c("b 5", "a 50", "c 9"))
})

test_that("solve count text needs one count of each kind", {
  expect_equal(solved_count_text(3, 10), "3 of 10 incidents solved")
  expect_null(solved_count_text(NA, 10))
  expect_null(solved_count_text(numeric(0), numeric(0)))
})

test_that("flagged agencies get a note under their escaped name", {
  cell <- agency_name_cell(c(NA, "no_clearances"))
  expect_equal(cell("A & B Police", 1), "A &amp; B Police")
  expect_match(cell("C Police", 2), "C Police<br>.*Reported no solved crimes")
  expect_equal(cell(NA, 1), "")
})

test_that("SHR ranking ignores hidden bars and puts fully hidden groups last", {
  data <- tibble(
    group_for_plot = c("State", "Region", "State", "Region", "State"),
    group = c("Small", "Small", "Gun", "Gun", "Tiny"),
    `Incidents reported` = c(4, 40, 50, 50, 2),
    `Incidents cleared` = c(4, 10, 25, 25, 2),
    clearance_rate = c(NA, 25, 50, 50, NA)
  )
  expect_equal(shr_category_order(data, list(sort_desc = TRUE)), c("Gun", "Small", "Tiny"))
})

test_that("SHR captions name hidden bars and skip the note when none are hidden", {
  spec <- list(category = "x", title = "T", note = "N")
  shown <- function(category) {
    tibble(group = c("A", "B"), `Incidents reported` = c(20, 30),
           `Incidents cleared` = c(10, 15), clearance_rate = c(50, 50), tooltip = c("a", "b"))
  }
  chart <- shr_panel_chart(spec, shown, "2022-2025")
  expect_no_match(chart$x$hc_opts$caption$text, "Not shown", fixed = TRUE)

  hidden <- function(category) mutate(shown(category), `Incidents reported` = c(4, 30),
                                      `Incidents cleared` = c(1, 15), clearance_rate = c(NA, 50))
  chart <- shr_panel_chart(spec, hidden, "2022-2025")
  expect_match(chart$x$hc_opts$caption$text, "Not shown, fewer than 10 homicides: A, 1 of 4 solved", fixed = TRUE)
  expect_equal(length(chart$x$hc_opts$series[[1]]$data), 1)
})

test_that("national SHR groups with few homicides are blanked", {
  data <- tibble(
    geo_abbr = "US", group_cat = "weapon", year = 2023,
    group = rep(c("Gun", "Rare"), each = 2),
    indicator = rep(c("Incidents reported", "Incidents cleared"), 2),
    n = c(100, 50, 5, 2)
  )
  actual <- function_shr_grouping_for_national_plot(data, "weapon", 2022)
  expect_true(is.na(actual$clearance_rate[actual$group == "Rare"]))
  expect_equal(actual$clearance_rate[actual$group == "Gun"], 50)
})
