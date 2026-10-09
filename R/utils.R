## functions and settings for highcharter plots

# set options to use comma separator for 1000s
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# set fonts to use as defaults
default_fonts <- c("GT-America", "sans-serif")
header_font <- c("GT-America", "sans-serif")
header_weight <- 700

# define justice reinvestment color palette
jr_pal <- c("#4095B1", "#273C4C", "#50A25D", "#E17619", "#E25449", "#779F38", "#AFABAB")

# define theme for highcharter
hc_theme_jc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    colors = jr_pal,
    chart = list(
      style = list(fontFamily = default_fonts)
    ),
    title = list(style = list(fontFamily = header_font, color = "#005FAD",
                              fontSize = "24px")),
    subtitle = list(style = list(fontFamily = default_fonts, fontSize = "16px",
                                 color = "#666666")),
    legend = list(align = "center", verticalAlign = "bottom"),
    caption = list(align = "right"),
    plotOptions = list(
      series = list(states = list(inactive = list(opacity = 1))),
      line = list(marker = list(enabled = TRUE)),
      spline = list(marker = list(enabled = TRUE)),
      area = list(marker = list(enabled = TRUE)),
      areaspline = list(marker = list(enabled = TRUE))
    )
  )
)

render_image <- JS("
  function(){
    this.renderer.image('https://csg-state-violent-crime.netlify.app/img/csgjc-logo.png', 30, this.chartHeight - 37, 140.1, 30)
    .add();
  }")

render_image_print <- JS("
  function(){
    logo=this.renderer.image('https://csg-state-violent-crime.netlify.app/img/csgjc-logo.png', 30, this.chartHeight - 37, 140.1, 30)
    .add(); this.print();
  }")

render_image_remove <- JS("function(){logo.element.remove();}")

# define default setup for highcharter plots
# add and configure exporting and accessibility modules
# set justice center theme
# set default tooltip text to be in input data column `tooltip`
hc_setup <- function(x) {
  hc_add_dependency(x, name = "modules/exporting.js") |>
    hc_add_dependency(name = "modules/offline-exporting.js") |>
    hc_add_dependency(name = "modules/accessibility.js") |>
    hc_exporting(
      enabled = TRUE,
      buttons = list(contextButton = list(menuItems = list("downloadPNG", "printChart"))),
      accessibility = list(enabled = TRUE)
    ) |>
    hc_add_theme(hc_theme_jc) |>
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |>
    hc_plotOptions(
      series = list(animation = FALSE),
      accessibility = list(
        enabled = TRUE,
        keyboardNavigation = list(enabled = TRUE)
      )
    ) |>
    hc_xAxis(
      title = "",
      labels = list(y = 25)
    ) |>
    hc_yAxis(
      title = "",
      labels = list(format = "{value:,.0f}")
    ) |>
  hc_exporting(
    chartOptions = list(
      chart = list(
        events = list(
          load = render_image
        )
      )
    )
  ) |>
  hc_chart(
    events = list(
      beforePrint = render_image_print,
      afterPrint = render_image_remove
    )
  )
}

# Keep chart-specific axis limits in one place. Values are in plotted units.
chart_bounds <- function(values, expansion = 0.5, floor = NULL, ceiling = NULL) {
  rng <- range(values, na.rm = TRUE)
  padding <- diff(rng) * expansion
  bounds <- c(rng[1] - padding, rng[2] + padding)
  if (!is.null(floor)) bounds[1] <- max(bounds[1], floor)
  if (!is.null(ceiling)) bounds[2] <- min(bounds[2], ceiling)
  bounds
}

change_map_bounds <- function(values) {
  extent <- max(abs(values), na.rm = TRUE)
  c(-extent, extent)
}

state_map_chart <- function(data, map, spec) {
  bounds <- if (isTRUE(spec$symmetric)) change_map_bounds(data[[spec$value]]) else NULL
  axis_colors <- spec$colors

  chart <- highchart() |>
    hc_add_series_map(
      map = map, df = data, joinBy = c("state_abb", "state_abbr"),
      value = spec$value, nullColor = "#E8E8E8",
      nullInteraction = isTRUE(spec$null_interaction),
      dataLabels = list(
        enabled = TRUE, format = "{point.state_abb}",
        style = list(fontSize = "13px", fontFamily = default_fonts,
                     fontWeight = 700, textOutline = 0)
      ),
      accessibility = list(point = list(valueDescriptionFormat = spec$accessibility))
    ) |>
    hc_title(text = spec$title, align = "left",
             style = list(fontFamily = header_font, color = "#005FAD",
                          fontSize = "24px", fontWeight = "bold")) |>
    hc_subtitle(text = spec$subtitle, align = "left",
                style = list(fontFamily = default_fonts, fontSize = "16px",
                             color = "#666666")) |>
    hc_colorAxis(
      min = if (is.null(bounds)) 0 else bounds[1],
      max = if (is.null(bounds)) NULL else bounds[2],
      endOnTick = TRUE, startOnTick = TRUE,
      stops = color_stops(length(axis_colors), axis_colors),
      labels = list(format = spec$label_format,
                    style = list(fontSize = "12px", fontFamily = default_fonts))
    ) |>
    hc_legend(align = "center", horizontalAlign = "bottom", layout = "horizontal",
              symbolHeight = 25, symbolWidth = spec$legend_width, x = 0, y = 0) |>
    hc_caption(text = spec$caption, align = "right",
               style = list(fontFamily = default_fonts)) |>
    hc_setup() |>
    hc_tooltip(style = list(fontFamily = default_fonts)) |>
    hc_plotOptions(series = list(
      states = list(inactive = list(opacity = 1)), cursor = "pointer",
      accessibility = list(enabled = TRUE,
                           keyboardNavigation = list(enabled = TRUE)),
      point = list(events = list(click = JS(
        "function(){window.top.location.href = this.options.url}"
      )))
    ))

  chart
}

offense_trend_chart <- function(data, offense, title, subtitle, color,
                                caption = "FBI UCR Program, state crime estimates") {
  data <- dplyr::filter(data, group == offense)
  bounds <- chart_bounds(data$incidents_reported_rate_total, floor = 0)

  data |>
    hchart("spline", hcaes(year, incidents_reported_rate_total),
           accessibility = list(point = list(
             valueDescriptionFormat = "{point.state}, year: {point.x:.0f}, rate: {point.y:.1f}"
           ))) |>
    hc_colors(color) |>
    hc_title(text = title) |>
    hc_subtitle(text = subtitle) |>
    hc_caption(text = caption) |>
    hc_legend(enabled = FALSE) |>
    hc_yAxis(min = bounds[1], max = bounds[2], endOnTick = FALSE) |>
    hc_setup()
}

metric_trend_chart <- function(data, value, title, subtitle, caption,
                               accessibility, expansion = 0.5, y_formatter = NULL) {
  bounds <- chart_bounds(data[[value]], expansion = expansion, floor = 0)
  chart <- data |>
    hchart("spline", hcaes(year, !!rlang::sym(value)), color = jr_pal[1],
           accessibility = list(point = list(valueDescriptionFormat = accessibility))) |>
    hc_title(text = title) |>
    hc_subtitle(text = subtitle) |>
    hc_caption(text = caption) |>
    hc_yAxis(min = bounds[1], max = bounds[2], endOnTick = FALSE) |>
    hc_legend(enabled = FALSE) |>
    hc_setup()

  if (!is.null(y_formatter)) {
    chart <- hc_yAxis(chart, labels = list(formatter = JS(y_formatter)))
  }
  chart
}

shr_rate_chart <- function(data, title, years, caption, categories = NULL) {
  chart <- data |>
    hchart("column", hcaes(group, clearance_rate),
           accessibility = list(point = list(
             valueDescriptionFormat = "{point.name}: {point.y:.1f}%"
           ))) |>
    hc_title(text = title) |>
    hc_subtitle(text = years) |>
    hc_caption(text = caption) |>
    hc_yAxis(min = 0, max = 100, endOnTick = FALSE) |>
    hc_legend(enabled = FALSE) |>
    hc_setup()

  if (!is.null(categories)) chart <- hc_xAxis(chart, categories = categories)
  chart
}

shr_panel_chart <- function(df, spec, first_year, years) {
  data <- function_shr_grouping_for_national_plot(df, spec$category, first_year)
  if (!is.null(spec$exclude)) data <- dplyr::filter(data, !group %in% spec$exclude)
  if (!is.null(spec$order)) {
    data$group <- factor(data$group, levels = spec$order)
    data <- dplyr::arrange(data, group)
  }
  if (isTRUE(spec$sort_desc)) data <- dplyr::arrange(data, dplyr::desc(clearance_rate))
  shr_rate_chart(data, spec$title, years, spec$caption, categories = spec$categories)
}

solve_rate_column <- function(offense, median_rate, min_width = 120, align = NULL) {
  colDef(
    name = paste0(offense, " Solve Rate<br><br>(State Average: ",
                  scales::percent(median_rate, accuracy = 1), ")"),
    html = TRUE,
    minWidth = min_width,
    align = align,
    format = colFormat(digits = 0, percent = TRUE),
    style = function(value) {
      color <- if (is.na(value)) "black" else if (value >= median_rate) "#15607A" else "#E17619"
      list(color = color, fontWeight = "bold")
    }
  )
}

offense_pal <- tibble(
  color = jr_pal[c(2,3,4,5)],
  crime = c("Homicide", "Robbery", "Rape", "Aggravated assault")
)

reactable_template <- function(df, sort_col = "rate", ...) {
  reactable(
    df,
    highlight = TRUE,
    searchable = TRUE,
    defaultSorted = sort_col,
    showPageSizeOptions	= TRUE,
    pageSizeOptions = c(10, 25, 100),
    defaultColDef = colDef(
      vAlign = "center",
      format = colFormat(digits = 0, separators = TRUE),
      headerStyle = list(fontWeight = 700, fontFamily = default_fonts,
                         fontVariant = "all-petite-caps"),
      style = list(fontWeight = 400, fontFamily = "Source Sans Pro")
    ),
    ...
  )
}

### signed whole number for table cells, e.g., "+3", "-5" or "0"
### missing values render as blank cells
format_signed <- function(value, suffix = "") {
  if (is.na(value)) return("")
  rounded <- round(value)
  sign <- if (rounded > 0) "+" else ""
  paste0(sign, rounded, suffix)
}

### reactable cell renderers take only the value
add_plus_sign_percent_change <- function(value) format_signed(value, "%")
add_plus_sign_percent_point_change <- function(value) format_signed(value)

### percent change from the first to the last year of a series
pct_change_first_last <- function(x, year) {
  (x[year == max(year)] - x[year == min(year)]) / x[year == min(year)]
}

### compare a change for text, e.g., "4 percent lower than"
### x is a proportion; unit "point" reads x as a percentage point change
### changes within half a percent or point read as "nearly the same as"
change_phrase <- function(x, unit = c("percent", "point")) {
  unit <- match.arg(unit)
  if (abs(x) <= 0.005) return("nearly the same as")

  amount <- scales::comma(abs(x) * 100, 1)
  word <- if (unit == "percent") {
    "percent"
  } else if (amount == "1") {
    "percentage point"
  } else {
    "percentage points"
  }
  paste(amount, word, if (x > 0) "higher than" else "lower than")
}

### "increased" or "decreased" for a numeric change; zero reads as "increased"
change_direction <- function(x, increase = "increased", decrease = "decreased") {
  if (x >= 0) increase else decrease
}

### create function to clean up and visualize SHR data
### this function will prepare a df for plotting -- grouping by
### varying incident or demographic characteristics
### df is fbi_shr_state.rds from jr_data_library; cat is a group_cat value
### pools years from first_year on and excludes unknown values
function_shr_grouping_for_national_plot <- function(df, cat, first_year){

  df |>
    filter(
      geo_abbr == "US",
      group_cat == cat,
      group != "Unknown",
      year >= first_year
    ) |>
    summarize(n = sum(n), .by = c(group, indicator)) |>
    pivot_wider(names_from = indicator, values_from = n) |>
    mutate(
      clearance_rate = `Incidents cleared` / `Incidents reported`,
      tooltip = paste0(
        "<b>","United States","–",group,"</b><br>",
        "Solve Rate: ", scales::percent(clearance_rate,
                                        accuracy = 1)),
      clearance_rate = clearance_rate*100
    )

}

function_shr_grouping_for_state_plot <- function(df, cat, state_abbr,
                                                 region_abbr, first_year,
                                                 region_label = "Other region states",
                                                 state_label = state_abbr) {
  df |>
    filter(
      geo_abbr %in% c(state_abbr, region_abbr),
      group_cat == cat,
      !group %in% c("Unknown", "Missing"),
      year >= first_year
    ) |>
    mutate(group_for_plot = if_else(geo_abbr == state_abbr, state_label, region_label)) |>
    summarize(n = sum(n, na.rm = TRUE), .by = c(group_for_plot, group, indicator)) |>
    pivot_wider(names_from = indicator, values_from = n) |>
    filter(!is.na(`Incidents reported`), `Incidents reported` > 0,
           !is.na(`Incidents cleared`)) |>
    mutate(
      group_for_plot = factor(group_for_plot, levels = c(state_label, region_label)),
      clearance_rate = `Incidents cleared` / `Incidents reported` * 100,
      tooltip = paste0(
        "<b>", group_for_plot, " – ", group, "</b><br>",
        "Solve Rate: ", scales::percent(clearance_rate / 100, accuracy = 1)
      )
    ) |>
    arrange(group_for_plot, group)
}
