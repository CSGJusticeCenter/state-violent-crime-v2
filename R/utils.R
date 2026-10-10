## functions and settings for highcharter plots

# set options to use comma separator for 1000s
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# set fonts to use as defaults
default_fonts <- c("GT-America", "sans-serif")
header_font <- c("GT-America", "sans-serif")
header_weight <- 700

### display formatters with the arguments of scales' comma(), percent() and
### dollar(), but halves round up (2.5 is 3, 0.625 is 63%) instead of to the
### even digit. They mask the scales versions once this file is sourced.
half_up <- function(x, accuracy) janitor::round_half_up(x / accuracy) * accuracy

comma <- function(x, accuracy = 1, scale = 1, ...) {
  scales::comma(half_up(x * scale, accuracy) / scale, accuracy = accuracy, scale = scale, ...)
}

percent <- function(x, accuracy = 1, scale = 100, ...) {
  scales::percent(half_up(x * scale, accuracy) / scale, accuracy = accuracy, scale = scale, ...)
}

dollar <- function(x, accuracy = 1, scale = 1, ...) {
  scales::dollar(half_up(x * scale, accuracy) / scale, accuracy = accuracy, scale = scale, ...)
}

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

### spec$tooltip names the tooltip_* column this map shows
state_map_chart <- function(data, map, spec) {
  if (!is.null(spec$tooltip)) {
    data$tooltip <- data[[spec$tooltip]]
    data <- dplyr::select(data, -dplyr::starts_with("tooltip_"))
  }
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

### spline trend by year, padded with chart_bounds()
### compare = TRUE draws one line per state_name, with the US dashed in gray,
### and shows a legend
### value_label and value_format describe each point to screen readers
trend_chart <- function(data, value, title, subtitle = NULL, caption, value_label,
                        value_format = "{point.y:.1f}", color = jr_pal[1],
                        compare = FALSE, legend = compare, expansion = 0.5,
                        floor = 0, ceiling = NULL, y_format = NULL, y_formatter = NULL) {
  bounds <- chart_bounds(data[[value]], expansion = expansion, floor = floor, ceiling = ceiling)
  accessibility <- list(point = list(valueDescriptionFormat = paste0(
    if (compare) "{point.series.name}, ", "year: {point.x:.0f}, ", value_label, ": ", value_format
  )))

  chart <- if (compare) {
    data$state_name <- forcats::fct_relevel(data$state_name, "United States", after = Inf)
    hchart(data, "spline", hcaes(year, !!rlang::sym(value), group = state_name),
           color = c(color, jr_pal[7]), dashStyle = c("solid", "dash"),
           opacity = c(1, 0.75), accessibility = accessibility)
  } else {
    hchart(data, "spline", hcaes(year, !!rlang::sym(value)),
           color = color, accessibility = accessibility)
  }

  chart <- chart |>
    hc_title(text = title) |>
    hc_caption(text = caption) |>
    hc_yAxis(min = bounds[1], max = bounds[2], endOnTick = FALSE)
  if (!is.null(subtitle)) chart <- hc_subtitle(chart, text = subtitle)
  if (!legend) chart <- hc_legend(chart, enabled = FALSE)

  ### label options merge into hc_setup()'s labels, so they go after it
  ### y_format replaces its format; y_formatter works alongside it because
  ### Highcharts uses formatter over format
  chart <- hc_setup(chart)
  if (!is.null(y_format)) chart <- hc_yAxis(chart, labels = list(format = y_format))
  if (!is.null(y_formatter)) chart <- hc_yAxis(chart, labels = list(formatter = JS(y_formatter)))
  chart
}

### reported rate trend for one offense
offense_trend_chart <- function(data, offense, title, subtitle, color,
                                caption = "FBI UCR Program, state crime estimates",
                                compare = FALSE) {
  trend_chart(
    dplyr::filter(data, group == offense), "incidents_reported_rate_total",
    title, subtitle, caption,
    value_label = paste(tolower(offense), "incidents per 100,000 residents"),
    color = color, compare = compare
  )
}

### homicide solve-rate panels by victim and incident characteristic
### note is the caption line above the data source
shr_panels <- list(
  race = list(
    category = "victim_race_eth", title = "Homicide solve rates by race and ethnicity",
    note = "This analysis excludes records where a victim's race and ethnicity is unknown"
  ),
  gender = list(
    category = "victim_sex", title = "Homicide solve rates by gender",
    note = "This analysis excludes records where a victim's gender is unknown"
  ),
  age = list(
    category = "victim_age", title = "Homicide solve rates by age",
    note = "This analysis excludes records where a victim's age is unknown",
    order = c("Under 25", "25 to 34", "35 to 45", "46+")
  ),
  weapon = list(
    category = "weapon", title = "Homicide solve rates by weapon",
    note = "This analysis includes the most common weapons across states in recent years",
    exclude = "Other", sort_desc = TRUE
  ),
  victims = list(
    category = "victim_count", title = "Homicide solve rates by number of victims",
    order = c("Single victim", "Multiple victims")
  )
)

shr_source <- function(years) {
  paste0("FBI UCR Program, Supplementary Homicide Reports (", years, ")")
}

### column chart of solve rates by group
### series names a column that splits the bars into series shown in a legend
shr_rate_chart <- function(data, title, years, caption, categories = NULL, series = NULL) {
  chart <- if (is.null(series)) {
    hchart(data, "column", hcaes(group, clearance_rate),
           accessibility = list(point = list(
             valueDescriptionFormat = "{point.name}: solve rate {point.y:.1f}%"
           )))
  } else {
    hchart(data, "column", hcaes(group, clearance_rate, group = !!rlang::sym(series)),
           accessibility = list(point = list(
             valueDescriptionFormat = "{point.series.name}, {point.name}: solve rate {point.y:.1f}%"
           )))
  }

  chart <- chart |>
    hc_title(text = title) |>
    hc_subtitle(text = years) |>
    hc_caption(text = caption) |>
    hc_yAxis(min = 0, max = 100, endOnTick = FALSE) |>
    hc_legend(enabled = !is.null(series)) |>
    hc_setup() |>
    hc_yAxis(labels = list(format = "{value}%"))

  if (!is.null(categories)) chart <- hc_xAxis(chart, categories = categories)
  chart
}

### x-axis order for an SHR panel, or NULL to keep the data's order
### sort_desc ranks groups by their solve rate pooled across all series,
### so every series shares one order
shr_category_order <- function(data, spec) {
  if (!is.null(spec$order)) return(intersect(spec$order, data$group))
  if (!isTRUE(spec$sort_desc)) return(NULL)
  data |>
    dplyr::summarize(
      rate = sum(`Incidents cleared`) / sum(`Incidents reported`),
      .by = group
    ) |>
    dplyr::arrange(dplyr::desc(rate)) |>
    dplyr::pull(group)
}

### one panel from shr_panels
### prep takes a group_cat value and returns grouped solve rates
shr_panel_chart <- function(spec, prep, years, series = NULL) {
  data <- prep(spec$category)
  if (!is.null(spec$exclude)) data <- dplyr::filter(data, !group %in% spec$exclude)

  categories <- shr_category_order(data, spec)
  if (!is.null(categories)) {
    data$group <- factor(data$group, levels = categories)
    data <- dplyr::arrange(data, group)
  }

  caption <- paste(c(spec$note, shr_source(years)), collapse = "<br>")
  shr_rate_chart(data, spec$title, years, caption, categories = categories, series = series)
}

### text colors for solve rates at or above and below a comparison rate
### both reach at least 4.5:1 contrast on white
solve_rate_colors <- c(above = "#15607A", below = "#B85A0D")

### whole percent for a solve rate, with halves rounded up (0.625 is 63)
solve_rate_percent <- function(x) half_up(x * 100, 1)

### solve rate as table text, e.g., "63%", or "" when missing
format_solve_rate <- function(x) {
  if (is.na(x)) "" else paste0(solve_rate_percent(x), "%")
}

### "above" or "below" for a solve rate against a comparison rate, or NA
### both are compared as the whole percents the cell and header show
solve_rate_side <- function(value, rate) {
  if (is.na(value) || is.na(rate)) return(NA_character_)
  if (solve_rate_percent(value) >= solve_rate_percent(rate)) "above" else "below"
}

### solve rate column colored against a comparison rate
### label names that rate in the header, e.g., "(U.S. Rate: 47%)"
### the solve-rate-above and solve-rate-below classes add arrows in styles.css,
### so the comparison doesn't rely on color alone
solve_rate_column <- function(offense, rate, label, min_width = 120, align = NULL) {
  colDef(
    name = paste0(offense, " Solve Rate<br><br>(", label, ": ", format_solve_rate(rate), ")"),
    html = TRUE,
    minWidth = min_width,
    align = align,
    cell = format_solve_rate,
    style = function(value) {
      side <- solve_rate_side(value, rate)
      list(color = if (is.na(side)) "black" else solve_rate_colors[[side]], fontWeight = "bold")
    },
    class = function(value) {
      side <- solve_rate_side(value, rate)
      if (is.na(side)) "" else paste0("solve-rate-", side)
    }
  )
}

### legend line for a solve-rate table, e.g., "▼ Orange indicates ..."
### the arrow is hidden from screen readers, which read the words
solve_rate_legend <- function(side, text) {
  arrow <- c(above = "▲", below = "▼")[[side]]
  name <- c(above = "Blue", below = "Orange")[[side]]
  paste0(
    '<span style="color:', solve_rate_colors[[side]], '">',
    '<span aria-hidden="true">', arrow, '</span> ', name, '</span> ', text
  )
}

### solve_rate_column() for each offense in layout, named solved_rate_<offense>
### rates is named by offense; layout gives each column's header and options
solve_rate_columns <- function(rates, label, layout) {
  purrr::imap(layout, \(options, offense) {
    rlang::exec(solve_rate_column, !!!options, rate = rates[[offense]], label = label)
  }) |>
    rlang::set_names(paste0("solved_rate_", names(layout)))
}

offense_pal <- tibble(
  color = jr_pal[c(2,3,4,5)],
  crime = c("Homicide", "Robbery", "Rape", "Aggravated assault")
)

offense_color <- function(offense) offense_pal$color[offense_pal$crime == offense]

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
  rounded <- half_up(value, 1)
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

### text helpers stop on missing input, so the failed page shows in the render log
check_change <- function(x) {
  if (length(x) != 1 || !is.finite(x)) {
    stop("Expected one finite change value, got ", deparse(x), call. = FALSE)
  }
}

### changes within half a percent or point read as about the same
is_about_same <- function(x) abs(x) <= 0.005

### compare a change for text, e.g., "4 percent lower than"
### x is a proportion; unit "point" reads x as a percentage point change
change_phrase <- function(x, unit = c("percent", "point")) {
  check_change(x)
  unit <- match.arg(unit)
  if (is_about_same(x)) return("nearly the same as")

  amount <- comma(abs(x) * 100, 1)
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
  check_change(x)
  if (x >= 0) increase else decrease
}

### describe a percent change, e.g., "increased by 18 percent" or "stayed about the same"
change_by <- function(x) {
  check_change(x)
  if (is_about_same(x)) return("stayed about the same")
  paste(change_direction(x), "by", comma(abs(x) * 100, 1), "percent")
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
        "Solve Rate: ", percent(clearance_rate,
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
        "Solve Rate: ", percent(clearance_rate / 100, accuracy = 1)
      )
    ) |>
    arrange(group_for_plot, group)
}
