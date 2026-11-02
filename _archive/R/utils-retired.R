# Retired helpers from R/utils.R. Kept for reference; nothing sources this file.

### create function to clean up and visualize SHR data
### this function will prepare a df for plotting -- grouping by
### varying incident or demographic characteristics
function_shr_grouping_for_plot <- function(df, var){

  df |>
    left_join(csg_regions_filtered,
              by = "state_abbr") |>
    ### use state name from shr df, not regions df
    ### we only use state name from regions df for filtering on correct region
    ### given a state name
    mutate(group_for_plot = case_when(state_abbr==state_abbr_params ~ csg_state_convert(state_abbr, "abbr", "name"),
                                      state_abbr!=state_abbr_params & !is.na(csg_region) ~ csg_region,
                                      TRUE ~ "drop")) |>
    filter(!.data[[var]] %in% c("Unknown", "Missing"),
           year>=2020,
           group_for_plot!="drop") |>
    dplyr::select(group_for_plot,
                  year,
                  n_total_incidents,
                  n_total_cleared,
                   all_of(var)) |>
    group_by(group_for_plot,
             .data[[var]]) |>
    ### sum across years by group
    summarize(n_total_cleared = sum(n_total_cleared, na.rm=TRUE),
              n_total_incidents = sum(n_total_incidents, na.rm=TRUE),
              clearance_rate = n_total_cleared/n_total_incidents) |>
    ungroup() |>
    # bind_rows(srs_by_cat_us) |>
    mutate(
      tooltip = paste0(
        "<b>",group_for_plot,"–",.data[[var]],"</b><br>",
        "Solve Rate: ", scales::percent(clearance_rate,
                                        accuracy = 1)),
      clearance_rate = clearance_rate*100
    )

}

## retired from the agency-level page, 2026-11-02; tests are in _archive/R/test-retired-city-charts.R

## funnel plot

### z scores for the funnel plot's inner and outer limits, two-sided 95% and 99.8%
funnel_z <- c(inner = qnorm(0.975), outer = qnorm(0.999))

### binomial limits around rate p for n incidents, at z standard errors
funnel_limits <- function(p, n, z) {
  se <- sqrt(p * (1 - p) / n)
  list(low = pmax(p - z * se, 0), high = p + z * se)
}

### legend names and colors for the funnel plot's three groups
funnel_names <- c(
  above = "Above the range expected by chance",
  within = "Within the range expected by chance",
  below = "Below the range expected by chance"
)
funnel_colors <- c(above = site_colors[["dark_blue"]], within = site_colors[["slate"]],
                   below = site_colors[["orange"]])

### agency rows for one offense with each solve rate's place against the outer
### funnel limits around us_rate
funnel_rows <- function(long, offense, us_rate) {
  rows <- agency_chart_rows(long, offense, us_rate)
  limits <- funnel_limits(us_rate, rows$n_reported, funnel_z[["outer"]])
  rows$funnel_side <- dplyr::case_when(
    rows$solved_rate > limits$high ~ "above",
    rows$solved_rate < limits$low ~ "below",
    .default = "within"
  )
  rows
}

### each agency's solve rate against its number of incidents, on a log scale,
### with the 95% (shaded) and 99.8% (dashed) ranges a rate would fall in by
### chance if the agency solved crimes at us_rate
agency_funnel_chart <- function(long, offense, us_rate, title, year) {
  rows <- funnel_rows(long, offense, us_rate)
  n_grid <- exp(seq(log(min(rows$n_reported) * 0.8), log(max(rows$n_reported) * 1.25), length.out = 80))
  inner <- funnel_limits(us_rate, n_grid, funnel_z[["inner"]])
  outer <- funnel_limits(us_rate, n_grid, funnel_z[["outer"]])
  band_line <- function(y, linked = NULL) {
    list(type = "line", data = purrr::map2(n_grid, y * 100, \(x, y) list(x, y)),
         name = "99.8% range expected by chance", linkedTo = linked, color = site_colors[["dark_gray"]],
         dashStyle = "ShortDash", lineWidth = 1, marker = list(enabled = FALSE),
         enableMouseTracking = FALSE, accessibility = list(enabled = FALSE))
  }
  points <- purrr::pmap(
    list(rows$n_reported, rows$solve_pct, rows$tooltip, rows$agency_label),
    \(x, y, tooltip, name) list(x = x, y = y, tooltip = tooltip, name = name)
  )
  agencies <- purrr::map(names(funnel_names), \(s) list(
    name = funnel_names[[s]], type = "scatter", color = funnel_colors[[s]], marker = agency_dot_marker,
    data = points[rows$funnel_side == s], zIndex = 5
  ))
  band <- list(
    type = "arearange", name = "95% range expected by chance",
    data = purrr::pmap(list(n_grid, inner$low * 100, inner$high * 100), \(x, low, high) list(x, low, high)),
    color = site_colors[["pale_blue"]], fillOpacity = 0.6, lineWidth = 0, marker = list(enabled = FALSE),
    enableMouseTracking = FALSE, accessibility = list(enabled = FALSE), zIndex = 0
  )
  us_pct <- solve_rate_percent(us_rate)
  label <- if (offense == total_label) "Violent incidents" else paste(offense, "incidents")

  highchart() |>
    hc_add_series_list(c(list(band, band_line(outer$high), band_line(outer$low, ":previous")), agencies)) |>
    hc_title(text = title) |>
    hc_subtitle(text = paste0(
      "Percentage of incidents solved by number of incidents reported, ", year,
      ". Agencies with fewer incidents vary more by chance alone.", agency_left_out_note(long, offense)
    )) |>
    hc_caption(text = paste(agency_chart_caption, year)) |>
    hc_setup() |>
    hc_xAxis(
      type = "logarithmic", gridLineWidth = 1, title = list(text = paste(label, "reported (log scale)")),
      labels = list(format = "{value:,.0f}", y = NULL)
    ) |>
    hc_yAxis(
      min = 0, max = solve_pct_max(rows$solve_pct), endOnTick = FALSE,
      title = list(text = "Solve rate"), labels = list(format = "{value}%"),
      plotLines = us_plot_line(us_pct, paste0("U.S. ", us_pct, "%"), align = "right", x = -4, y = -6)
    ) |>
    hc_plotOptions(scatter = list(accessibility = list(point = list(
      valueDescriptionFormat = paste0("{point.name}, ", tolower(label), ": {point.x}, solve rate: {point.y}%")
    )))) |>
    hc_size(height = 560)
}

### counts of agencies above and below the outer funnel limits for an offense
funnel_counts <- function(long, offense, us_rate) {
  sides <- funnel_rows(long, offense, us_rate)$funnel_side
  c(above = sum(sides == "above"), below = sum(sides == "below"), n = length(sides))
}

## small multiples

### side-by-side panels, one per offense, with every agency's solve rate on the
### same scale and agencies in the same order, highest violent crime solve rate
### first; us_rates is named by offense
agency_offense_panels <- function(long, us_rates, title, year, offenses = unname(table_offenses)) {
  order <- agency_chart_rows(long, total_label, NA)$agency_label
  panel_rows <- purrr::map(offenses, \(offense) {
    agency_chart_rows(long, offense, us_rates[[offense]]) |>
      dplyr::mutate(x = match(agency_label, order) - 1)
  })
  max_pct <- solve_pct_max(unlist(purrr::map(panel_rows, "solve_pct")))
  gap <- 3
  width <- (100 - gap * (length(offenses) - 1)) / length(offenses)

  y_axes <- purrr::imap(offenses, \(offense, i) {
    us_pct <- solve_rate_percent(us_rates[[offense]])
    list(
      left = paste0((i - 1) * (width + gap), "%"), width = paste0(width, "%"), offset = 0,
      min = 0, max = max_pct, tickInterval = 50, endOnTick = FALSE, opposite = TRUE, gridLineWidth = 1,
      title = list(text = offense, style = list(fontWeight = 700, color = site_colors[["text"]])),
      labels = list(format = "{value}%", style = list(fontSize = "11px")),
      plotLines = us_plot_line(us_pct, paste0("U.S. ", us_pct, "%"), x = 3, y = 14)
    )
  })
  series <- purrr::imap(panel_rows, \(rows, i) {
    points <- purrr::pmap(list(rows$x, rows$solve_pct, rows$tooltip), \(x, y, tooltip) list(x = x, y = y, tooltip = tooltip))
    purrr::map(c("above", "below"), \(s) list(
      name = agency_side_names[[s]], type = "scatter", yAxis = i - 1, color = solve_rate_colors[[s]],
      marker = list(radius = 3.5, symbol = "circle"), data = points[rows$side == s],
      id = if (i == 1) s, linkedTo = if (i > 1) s,
      accessibility = list(point = list(valueDescriptionFormat = paste0(
        "{point.category}, ", tolower(offenses[i]), " solve rate: {point.y}%"
      )))
    ))
  }) |>
    purrr::list_flatten()
  series <- purrr::map(series, \(s) Filter(Negate(is.null), s))

  chart <- thin_bar_chart(series, order, "{point.category}") |>
    hc_title(text = title) |>
    hc_subtitle(text = paste0(
      "Percentage of incidents solved by offense, ", year,
      ". Agencies are in the same order in every panel, from highest to lowest violent crime solve rate.",
      " Panels leave out rates from fewer than ", min_rate_reported, " incidents."
    )) |>
    hc_caption(text = paste(agency_chart_caption, year))
  chart$x$hc_opts$yAxis <- y_axes
  chart
}

### counts of agencies at or above the U.S. rate in every offense, and below it
### in every offense, among agencies with a shown rate for each
agencies_consistent <- function(long, us_rates, offenses = unname(table_offenses)) {
  sides <- purrr::map(offenses, \(o) dplyr::select(agency_chart_rows(long, o, us_rates[[o]]), ori, side)) |>
    purrr::list_rbind() |>
    dplyr::summarize(n = dplyr::n(), above = sum(side == "above"), .by = ori) |>
    dplyr::filter(n == length(offenses))
  c(all_above = sum(sides$above == length(offenses)), all_below = sum(sides$above == 0), n = nrow(sides))
}

