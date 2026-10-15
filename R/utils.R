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

### crime rates per 100,000 residents show whole numbers, except homicide
### rates, which are small and show one decimal; zero is "0" and missing is ""
format_rate_100k <- function(x, homicide = FALSE) {
  homicide <- rep_len(homicide, length(x))
  out <- ifelse(homicide, comma(x, 0.1), comma(x, 1))
  out[out == "0.0"] <- "0"
  ifelse(is.na(x), "", out)
}

### table cells show missing values as a dash, explained in a note under each table
### screen readers skip the dash, so they read "not available" in its place
na_text <- paste0(
  '<span aria-hidden="true">—</span>',
  '<span class="visually-hidden">not available</span>'
)

### reactable cell functions for rate columns, so tables match tooltips
### reactable also passes the row index, which these ignore
rate_100k_cell <- function(value, ...) {
  if (is.na(value)) na_text else format_rate_100k(value)
}
homicide_rate_100k_cell <- function(value, ...) {
  if (is.na(value)) na_text else format_rate_100k(value, homicide = TRUE)
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
    ### legend items only label the chart, so they don't look or act like buttons
    legend = list(
      align = "center", verticalAlign = "bottom",
      itemStyle = list(cursor = "default"),
      itemHoverStyle = list(color = "#333333", cursor = "default"),
      accessibility = list(enabled = FALSE)
    ),
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

### cancel legend clicks, which would hide series or map color classes
lock_legend <- JS("
  function() {
    this.legend.allItems.forEach(function(item) {
      Highcharts.addEvent(item, 'legendItemClick', function(e) { e.preventDefault(); });
    });
  }")

# define default setup for highcharter plots
# add and configure exporting and accessibility modules
# set justice center theme
# set default tooltip text to be in input data column `tooltip`
hc_setup <- function(x) {
  ### describe every point to screen readers, even on maps with over 200 counties
  x$x$hc_opts$accessibility$series$pointDescriptionEnabledThreshold <- FALSE

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
      load = lock_legend,
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

### signed whole number as plain text, e.g., "+3%", "-5%" or "0%"
signed_text <- function(x, suffix = "") {
  rounded <- half_up(x, 1)
  ifelse(is.na(x), NA_character_, paste0(ifelse(rounded > 0, "+", ""), rounded, suffix))
}

### class index for a change map, from 1 for the largest drop to 2 * length(breaks) + 1
### for the largest rise; whole-number changes under breaks[1] in size share the
### middle class, and each break starts a class on both sides, so -5 and +5 both
### leave it
change_map_class <- function(x, breaks) {
  rounded <- half_up(x, 1)
  as.integer(length(breaks) + 1 + sign(rounded) * findInterval(abs(rounded), breaks))
}

### colorAxis dataClasses for change_map_class(), labeled with signed whole numbers
change_map_data_classes <- function(breaks, colors, suffix = "") {
  n <- length(breaks)
  stopifnot(n >= 2, length(colors) == 2 * n + 1)
  label <- \(x) signed_text(x, suffix)
  inner <- breaks - 1
  labels <- c(
    paste(label(-breaks[n]), "or lower"),
    paste(label(-inner[n:2]), "to", label(-breaks[(n - 1):1])),
    paste(label(-inner[1]), "to", label(inner[1])),
    paste(label(breaks[1:(n - 1)]), "to", label(inner[2:n])),
    paste(label(breaks[n]), "or higher")
  )
  lapply(seq_along(colors), \(i) list(from = i, to = i, color = colors[i], name = labels[i]))
}

### colorAxis dataClasses from the breaks between classes, one color per class
### breaks sit at the low end of each class, so 45 falls in "45–55%"
map_data_classes <- function(breaks, colors, suffix = "") {
  stopifnot(length(colors) == length(breaks) + 1)
  n <- length(breaks)
  labels <- c(
    paste0("Under ", breaks[1], suffix),
    paste0(breaks[-n], "–", breaks[-1], suffix),
    paste0(breaks[n], suffix, " or more")
  )
  from <- c(NA, breaks)
  to <- c(breaks, NA)
  lapply(seq_along(colors), \(i) {
    Filter(Negate(is.na), list(from = from[i], to = to[i], color = colors[i], name = labels[i]))
  })
}

### dark border on every map legend square, so white and pale classes show on the white page
outline_legend_symbols <- JS("
  function() {
    this.legend.allItems.forEach(function(item) {
      if (item.legendSymbol) item.legendSymbol.attr({ stroke: '#333333', 'stroke-width': 1 });
    });
  }")

### spec$tooltip names the tooltip_* column this map shows
### spec$breaks bins a change into change_map_class() classes, and screen
### readers read the change as change_text, e.g., "+5%"
state_map_chart <- function(data, map, spec) {
  if (!is.null(spec$tooltip)) {
    data$tooltip <- data[[spec$tooltip]]
    data <- dplyr::select(data, -dplyr::starts_with("tooltip_"))
  }
  binned <- !is.null(spec$breaks)
  value <- spec$value
  if (binned) {
    data$change_class <- change_map_class(data[[spec$value]], spec$breaks)
    data$change_text <- signed_text(data[[spec$value]], spec$suffix %||% "")
    value <- "change_class"
  }

  chart <- highchart() |>
    hc_add_series_map(
      map = map, df = data, joinBy = c("state_abb", "state_abbr"),
      value = value, nullColor = "#E8E8E8",
      nullInteraction = isTRUE(spec$null_interaction),
      dataLabels = list(
        enabled = TRUE, format = "{point.state_abb}",
        style = list(fontSize = "13px", fontFamily = default_fonts,
                     fontWeight = 700, textOutline = 0)
      ),
      accessibility = list(point = list(valueDescriptionFormat = spec$accessibility))
    ) |>
    hc_title(text = spec$title) |>
    hc_subtitle(text = spec$subtitle) |>
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

  if (binned) {
    chart |>
      hc_colorAxis(dataClasses = change_map_data_classes(spec$breaks, spec$colors, spec$suffix %||% "")) |>
      hc_legend(align = "center", verticalAlign = "bottom", layout = "horizontal",
                symbolRadius = 0, symbolHeight = 14, symbolWidth = 14) |>
      hc_chart(events = list(render = outline_legend_symbols))
  } else {
    chart |>
      hc_colorAxis(
        min = 0, endOnTick = TRUE, startOnTick = TRUE,
        stops = color_stops(length(spec$colors), spec$colors),
        labels = list(format = spec$label_format,
                      style = list(fontSize = "12px", fontFamily = default_fonts))
      ) |>
      hc_legend(align = "center", horizontalAlign = "bottom", layout = "horizontal",
                symbolHeight = 25, symbolWidth = spec$legend_width, x = 0, y = 0)
  }
}

### spline trend by year, padded with chart_bounds()
### compare = TRUE draws one line per state_name, with the US dashed in gray,
### and labels each line's end with its name and label_format value in place
### of a legend
### value_label and value_format describe each point to screen readers
trend_chart <- function(data, value, title, subtitle = NULL, caption, value_label,
                        value_format = "{point.y:.1f}", label_format = value_format,
                        color = jr_pal[1], compare = FALSE, expansion = 0.5,
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
    hc_yAxis(min = bounds[1], max = bounds[2], endOnTick = FALSE) |>
    hc_legend(enabled = FALSE)
  if (!is.null(subtitle)) chart <- hc_subtitle(chart, text = subtitle)

  ### label options merge into hc_setup()'s labels, so they go after it
  ### y_format replaces its format; y_formatter works alongside it because
  ### Highcharts uses formatter over format
  chart <- hc_setup(chart)
  if (compare) {
    chart <- compare_end_labels(chart, label_format) |>
      hc_chart(marginRight = 100, events = list(render = end_labels))
  }
  if (!is.null(y_format)) chart <- hc_yAxis(chart, labels = list(format = y_format))
  if (!is.null(y_formatter)) chart <- hc_yAxis(chart, labels = list(formatter = JS(y_formatter)))
  chart
}

### hollow markers on every point in year, outlined in the series color
### series without their own color take it from the chart's hc_colors() palette
hc_hollow_year <- function(chart, year) {
  series_list <- chart$x$hc_opts$series
  palette <- chart$x$hc_opts$colors
  chart$x$hc_opts$series <- lapply(seq_along(series_list), function(i) {
    series <- series_list[[i]]
    color <- series$color %||% palette[(i - 1) %% length(palette) + 1]
    if (length(color) != 1) stop("hc_hollow_year() needs a color for every series")
    series$data <- lapply(series$data, function(point) {
      if (isTRUE(point$x == year)) {
        point$marker <- list(fillColor = "#FFFFFF", lineColor = color, lineWidth = 2)
      }
      point
    })
    series
  })
  chart
}

### labels at the right end of each series in place of a legend
### a series' custom options can set label (default its name), short (used on
### charts narrower than 500px), value (a format string for a second line) and
### color (default the series color, darkened so text stays readable)
### lines start at their last point and stacked areas at the middle of their
### band; close labels push apart, and the right margin widens to fit them
end_labels <- JS("
  function() {
    var chart = this, lineHeight = 14, pad = 3;
    var narrow = chart.chartWidth < 500;
    (chart.endLabels || []).forEach(function(label) { label.destroy(); });
    var items = chart.series.filter(function(s) { return s.visible && s.points.length; }).map(function(s) {
      var p = s.points[s.points.length - 1];
      var custom = s.options.custom || {};
      var lines = [(narrow && custom.short) || custom.label || s.name];
      if (custom.value) lines.push(Highcharts.format(custom.value, { point: p, series: s }));
      var y = p.plotY;
      if (s.type === 'area' && p.stackY !== undefined) {
        y = (p.plotY + chart.yAxis[0].toPixels(p.stackY - p.y, true)) / 2;
      }
      var height = lines.length * lineHeight;
      var color = custom.color || Highcharts.color(s.color).brighten(-0.2).get();
      return { lines: lines, color: color, height: height, top: y - height / 2 };
    }).sort(function(a, b) { return a.top - b.top; });
    items.forEach(function(item, i) {
      if (i > 0) item.top = Math.max(item.top, items[i - 1].top + items[i - 1].height + pad);
    });
    var last = items[items.length - 1];
    var overflow = last ? last.top + last.height - chart.plotHeight : 0;
    if (overflow > 0) items.forEach(function(item) { item.top -= overflow; });
    chart.endLabels = items.map(function(item) {
      return chart.renderer
        .text(item.lines.join('<br>'), chart.plotLeft + chart.plotWidth + 8, chart.plotTop + item.top + 11)
        .css({ color: item.color, fontSize: '12px', fontWeight: 600, lineHeight: lineHeight + 'px' })
        .add();
    });
    var width = Math.max.apply(null, chart.endLabels.map(function(label) { return label.getBBox().width; }).concat(0));
    var margin = Math.ceil(width) + 16;
    if (Math.abs(margin - (chart.options.chart.marginRight || 0)) > 1) {
      chart.update({ chart: { marginRight: margin } });
    }
  }")

### short end labels for state comparison charts on narrow screens
state_short_names <- c(rlang::set_names(state.abb, state.name), "District of Columbia" = "DC")

### custom options end_labels reads for a state and U.S. comparison; value is
### the format string for each label's second line
### the U.S. reads "U.S." at every width, in a gray dark enough for text
compare_end_labels <- function(chart, value) {
  chart$x$hc_opts$series <- lapply(chart$x$hc_opts$series, function(series) {
    series$custom <- if (series$name == "United States") {
      list(label = "U.S.", value = value, color = "#6F6B6B")
    } else {
      list(short = unname(state_short_names[series$name]), value = value)
    }
    series
  })
  chart
}

### stacked area of offense rates per 100k, labeled at the right end
### data needs rate_label, the rate as tooltips show it, for screen readers
### colors follow the order of the group levels
offense_area_chart <- function(data, title, colors,
                               caption = "FBI UCR Program, state crime estimates") {
  hchart(data, "area", hcaes(year, incidents_reported_rate_total, group = group),
         stacking = "normal",
         accessibility = list(point = list(
           valueDescriptionFormat = "{point.series.name}, year: {point.x:.0f}, incidents per 100,000 residents: {point.rate_label}"
         ))) |>
    hc_colors(colors) |>
    hc_title(text = title) |>
    hc_subtitle(text = "Crime reported to police per 100,000 residents") |>
    hc_caption(text = caption) |>
    hc_legend(enabled = FALSE) |>
    hc_plotOptions(area = list(lineWidth = 1, lineColor = "#FFFFFF", fillOpacity = 0.9,
                               marker = list(enabled = FALSE))) |>
    hc_chart(marginRight = 120) |>
    hc_setup() |>
    hc_chart(events = list(render = end_labels))
}

### reported rate trend for one offense
### data needs rate_label, the rate as tooltips show it, for screen readers
offense_trend_chart <- function(data, offense, title, subtitle, color,
                                caption = "FBI UCR Program, state crime estimates",
                                compare = FALSE) {
  trend_chart(
    dplyr::filter(data, group == offense), "incidents_reported_rate_total",
    title, subtitle, caption,
    value_label = paste(tolower(offense), "incidents per 100,000 residents"),
    value_format = "{point.rate_label}",
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

### state bars in the trend charts' state color, region bars in a muted gray
### that keeps 3:1 contrast on white
shr_series_colors <- c(jr_pal[1], "#8C8C8C")

### column chart of solve rates by group
### series names a column that splits the bars into series shown in a legend,
### colored in order by shr_series_colors
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
           ))) |>
      hc_colors(shr_series_colors)
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
### sort_desc ranks groups by their solve rate pooled across the bars shown in
### all series, so every series shares one order; groups with no bar go last
shr_category_order <- function(data, spec) {
  if (!is.null(spec$order)) return(intersect(spec$order, data$group))
  if (!isTRUE(spec$sort_desc)) return(NULL)
  shown <- if ("clearance_rate" %in% names(data)) !is.na(data$clearance_rate) else TRUE
  ranked <- data[shown, ] |>
    dplyr::summarize(
      rate = sum(`Incidents cleared`) / sum(`Incidents reported`),
      .by = group
    ) |>
    dplyr::arrange(dplyr::desc(rate)) |>
    dplyr::pull(group)
  c(ranked, setdiff(unique(data$group), ranked))
}

### caption line naming SHR bars hidden for few homicides, or NULL
### series names the column whose value labels each hidden bar
shr_hidden_note <- function(data, series = NULL) {
  hidden <- data[is.na(data$clearance_rate), ]
  if (nrow(hidden) == 0) return(NULL)
  label <- as.character(hidden$group)
  if (!is.null(series)) label <- paste0(label, " (", hidden[[series]], ")")
  paste0(
    "Not shown, fewer than ", min_rate_reported, " homicides: ",
    paste0(label, ", ", comma(hidden$`Incidents cleared`), " of ",
           comma(hidden$`Incidents reported`), " solved", collapse = "; ")
  )
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

  hidden_note <- shr_hidden_note(data, series)

  ### with one series, a hidden group's empty slot is dropped; with several,
  ### the slot stays so the other series keeps its bar
  if (is.null(series)) {
    data <- dplyr::filter(data, !is.na(clearance_rate))
    if (!is.null(categories)) categories <- intersect(categories, as.character(data$group))
  }

  caption <- paste(c(spec$note, hidden_note, shr_source(years)), collapse = "<br>")
  shr_rate_chart(data, spec$title, years, caption, categories = categories, series = series)
}

### text and pill fill colors for solve rates at or above and below a
### comparison rate; each text color reaches at least 4.5:1 contrast on its fill
### below pills are filled and above pills are outlined, so the two differ
### without color; the outline is in styles.css
solve_rate_colors <- c(above = "#15607A", below = "#9A4A0A")
solve_rate_fills <- c(above = "#FFFFFF", below = "#FBE8DA")

### text in a pill styled for one side of the comparison rate
### sr_text adds words that only screen readers announce; title shows on hover
solve_rate_pill <- function(text, side, sr_text = NULL, title = NULL) {
  paste0(
    '<span class="solve-rate-pill solve-rate-pill-', side,
    '" style="color:', solve_rate_colors[[side]],
    ";background-color:", solve_rate_fills[[side]], '"',
    title_attr(title), ">",
    if (!is.null(sr_text)) paste0('<span class="visually-hidden">', sr_text, "</span>"),
    text, "</span>"
  )
}

### HTML title attribute, or "" when title is NULL
title_attr <- function(title) {
  if (is.null(title)) "" else paste0(' title="', htmltools::htmlEscape(title, attribute = TRUE), '"')
}

### "5 of 8 incidents solved" for a solve-rate cell's hover text, or NULL
solved_count_text <- function(solved, reported) {
  if (length(solved) != 1 || length(reported) != 1 || is.na(solved) || is.na(reported)) return(NULL)
  paste(comma(solved), "of", comma(reported), "incidents solved")
}

### gray cell text for a rate from few incidents, with words that tell screen
### readers why it's gray; blank and missing text stay as they are
small_n_text <- function(text, min_reported = min_rate_reported, title = NULL) {
  if (text %in% c("", na_text)) return(text)
  paste0(
    '<span class="small-n-note"', title_attr(title), ">", text,
    '<span class="visually-hidden"> (fewer than ', min_reported, " incidents)</span></span>"
  )
}

### reactable cell function that formats a value and grays it in rows with
### few incidents; reported holds each row's count in the table's data order
small_n_cell <- function(reported, format, min_reported = min_rate_reported) {
  function(value, index) {
    text <- format(value)
    if (isTRUE(reported[index] < min_reported)) small_n_text(text, min_reported) else text
  }
}

### notes under agency names, keyed by data_flag from R/agency-county-data.R
agency_flag_notes <- c(
  no_clearances = "Reported no solved crimes",
  no_violent_crime = "Reported no violent crime"
)

### agency name cell with a gray note under flagged agencies
### flags holds each row's data_flag in the table's data order
agency_name_cell <- function(flags) {
  function(value, index) {
    name <- if (is.na(value)) "" else htmltools::htmlEscape(value)
    flag <- flags[index]
    if (is.na(flag)) return(name)
    paste0(name, '<br><span class="agency-flag">', agency_flag_notes[[flag]], "</span>")
  }
}

### whole percent for a solve rate, with halves rounded up (0.625 is 63)
solve_rate_percent <- function(x) half_up(x * 100, 1)

### solve rate as table text, e.g., "63%", or a dash when missing
format_solve_rate <- function(x) {
  if (is.na(x)) na_text else paste0(solve_rate_percent(x), "%")
}

### solve rate as page text, e.g., "63 percent", rounded as in tables
solve_rate_text <- function(x) {
  check_change(x)
  paste(solve_rate_percent(x), "percent")
}

### "above" or "below" for a solve rate against a comparison rate, or NA
### both are compared as the whole percents the cell and header show
solve_rate_side <- function(value, rate) {
  if (is.na(value) || is.na(rate)) return(NA_character_)
  if (solve_rate_percent(value) >= solve_rate_percent(rate)) "above" else "below"
}

### solve rate column with each rate in a pill tinted against a comparison rate
### the comparison rate shows in the footer; rates without one show as plain text
### reported and solved hold each row's counts in the table's data order; with
### them, cells show the counts on hover, and rates from fewer than
### min_reported incidents show as gray text without a pill
### index is unused without reported, so tests can call cell(value)
solve_rate_column <- function(rate, reported = NULL, solved = NULL,
                              min_reported = min_rate_reported) {
  sr_text <- c(above = "at or above, ", below = "below, ")
  colDef(
    name = "Solve rate",
    html = TRUE,
    footer = format_solve_rate(rate),
    cell = function(value, index) {
      text <- format_solve_rate(value)
      n <- if (is.null(reported)) NA else reported[index]
      title <- if (is.na(value) || is.null(solved)) NULL else solved_count_text(solved[index], n)
      if (isTRUE(n < min_reported)) return(small_n_text(text, min_reported, title))
      side <- solve_rate_side(value, rate)
      if (is.na(side)) text else solve_rate_pill(text, side, sr_text[[side]], title)
    }
  )
}

### legend line for a solve-rate table, e.g., "[Orange] indicates ..."
solve_rate_legend <- function(side, text) {
  name <- c(above = "Blue", below = "Orange")[[side]]
  paste(solve_rate_pill(name, side), text)
}

### violent offenses in table column order, named by snake_case offense
table_offenses <- c(
  aggravated_assault = "Aggravated assault",
  homicide = "Homicide",
  rape = "Rape",
  robbery = "Robbery"
)

### an offense's per-100k and solve rate column names
offense_pair <- function(offense) paste0(c("n_reported_rate_", "solved_rate_"), offense)

### every offense's column names in table order, for selecting table data
offense_column_names <- function(offenses = table_offenses) {
  unlist(lapply(names(offenses), offense_pair))
}

### per-100k and solve rate columns for each offense, in pairs
### columns are n_reported_rate_<offense> and solved_rate_<offense>
### rates is named by offense and holds the comparison solve rates
### crime_rates, when given, is named by offense and holds the comparison
### rates per 100k, which show in the footer beside the solve rates
### counts, when given, is the table's data with n_reported_<offense> and
### n_solved_<offense> columns
offense_columns <- function(rates, crime_rates = NULL, offenses = table_offenses,
                            counts = NULL) {
  columns <- purrr::imap(offenses, \(label, offense) {
    rate_cell <- if (offense == "homicide") homicide_rate_100k_cell else rate_100k_cell
    footer <- if (!is.null(crime_rates)) rate_cell(unname(crime_rates[offense]))
    list(
      colDef(name = "Per 100k", cell = rate_cell, footer = footer),
      solve_rate_column(
        rates[[offense]],
        reported = counts[[paste0("n_reported_", offense)]],
        solved = counts[[paste0("n_solved_", offense)]]
      )
    ) |>
      rlang::set_names(offense_pair(offense))
  })
  purrr::list_flatten(unname(columns))
}

### one header group per offense over its offense_columns() pair
offense_column_groups <- function(offenses = table_offenses) {
  purrr::imap(offenses, \(label, offense) colGroup(name = label, columns = offense_pair(offense))) |>
    unname()
}

### label for a state's total row, e.g., "NY state total" or "DC total"
state_total_label <- function(abbr) {
  if (abbr == "DC") "DC total" else paste(abbr, "state total")
}

### minimum column widths, in pixels, shared by every table
table_widths <- list(name = 200, county = 130, population = 120, number = 100)

### first column with the row names, kept in view while the table scrolls
### footer labels the total row, e.g., "U.S. total"
### a right border sets off the sticky column
sticky_name_column <- function(name, footer = NULL, ...) {
  border <- list(borderRight = "1px solid #eee")
  colDef(
    name = name, footer = footer, minWidth = table_widths$name, sticky = "left",
    style = border, headerStyle = border, footerStyle = border, ...
  )
}

offense_pal <- tibble(
  color = jr_pal[c(2,3,4,5)],
  crime = c("Homicide", "Robbery", "Rape", "Aggravated assault")
)

offense_color <- function(offense) offense_pal$color[offense_pal$crime == offense]

### fonts and header styles live in the theme, so column styles add to them
### tabular figures give every digit the same width, so numbers line up in columns
reactable_theme <- reactableTheme(
  style = list(fontFamily = paste(default_fonts, collapse = ", "),
               fontVariantNumeric = "tabular-nums"),
  headerStyle = list(fontWeight = 700),
  groupHeaderStyle = list(fontWeight = 700),
  footerStyle = list(fontWeight = 700)
)

### default table cell: whole numbers with commas, escaped text, or the missing-value dash
default_cell <- function(value, ...) {
  if (is.na(value)) return(na_text)
  if (is.numeric(value)) comma(value) else htmltools::htmlEscape(value)
}

### cells render as HTML so missing values can carry screen reader text
### missing values sort last in both directions
reactable_template <- function(df, sort_col = "rate", ...) {
  reactable(
    df,
    highlight = TRUE,
    searchable = TRUE,
    defaultSorted = sort_col,
    showPageSizeOptions	= TRUE,
    pageSizeOptions = c(10, 25, 100),
    theme = reactable_theme,
    defaultColDef = colDef(
      minWidth = table_widths$number,
      vAlign = "center",
      headerVAlign = "bottom",
      html = TRUE,
      sortNALast = TRUE,
      cell = default_cell
    ),
    ...
  )
}

### signed whole number for table cells, e.g., "+3", "-5" or "0"
### missing values show as a dash
format_signed <- function(value, suffix = "") {
  if (is.na(value)) return(na_text)
  rounded <- half_up(value, 1)
  sign <- if (rounded > 0) "+" else ""
  paste0(sign, rounded, suffix)
}

### reactable cell renderers take only the value
add_plus_sign_percent_change <- function(value) format_signed(value, "%")
add_plus_sign_percent_point_change <- function(value) format_signed(value)

### percent change in x from year `from` to year `to`
### stops unless each year appears exactly once, so a missing year can't read as no change
pct_change_between <- function(x, year, from, to) {
  for (y in c(from, to)) {
    if (sum(year == y, na.rm = TRUE) != 1) {
      stop("Expected one ", y, " value for a percent change, found ", sum(year == y, na.rm = TRUE), call. = FALSE)
    }
  }
  (x[year %in% to] - x[year %in% from]) / x[year %in% from]
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
      clearance_rate = `Incidents cleared` / `Incidents reported` * 100,
      tooltip = shr_tooltip("United States", group, clearance_rate,
                            `Incidents cleared`, `Incidents reported`)
    ) |>
    hide_small_shr_groups()

}

### SHR bar tooltip with the solve rate and the counts behind it
shr_tooltip <- function(label, group, rate, cleared, reported) {
  paste0(
    "<b>", label, " – ", group, "</b><br>",
    "Solve rate: ", percent(rate / 100, accuracy = 1), "<br>",
    "Homicides solved: ", comma(cleared), " of ", comma(reported)
  )
}

### blank solve rates from fewer than min_reported homicides, so their bars drop
hide_small_shr_groups <- function(data, min_reported = min_rate_reported) {
  mutate(data, clearance_rate = if_else(`Incidents reported` < min_reported, NA_real_, clearance_rate))
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
      tooltip = shr_tooltip(group_for_plot, group, clearance_rate,
                            `Incidents cleared`, `Incidents reported`)
    ) |>
    hide_small_shr_groups() |>
    arrange(group_for_plot, group)
}
