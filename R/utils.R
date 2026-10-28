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

### default series colors, in order, from site_colors in R/page-data.R
chart_pal <- unname(site_colors[c("blue", "navy", "slate", "teal", "dark_blue", "muted")])

# define theme for highcharter
hc_theme_jc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    colors = chart_pal,
    chart = list(
      style = list(fontFamily = default_fonts, color = site_colors[["muted"]])
    ),
    title = list(style = list(fontFamily = header_font, color = site_colors[["text"]],
                              fontSize = "24px")),
    subtitle = list(style = list(fontFamily = default_fonts, fontSize = "16px",
                                 color = site_colors[["muted"]])),
    ### hc_theme_smpl() sets light gray axis lines; these replace them
    xAxis = list(lineColor = site_colors[["baseline"]], tickColor = site_colors[["baseline"]],
                 gridLineColor = site_colors[["grid"]], minorGridLineColor = site_colors[["grid"]],
                 labels = list(style = list(color = site_colors[["muted"]]))),
    yAxis = list(gridLineColor = site_colors[["grid"]], lineColor = site_colors[["grid"]],
                 minorGridLineColor = site_colors[["grid"]], tickColor = site_colors[["grid"]],
                 labels = list(style = list(color = site_colors[["muted"]]))),
    ### legend items only label the chart, so they don't look or act like buttons
    legend = list(
      align = "center", verticalAlign = "bottom",
      itemStyle = list(cursor = "default"),
      itemHoverStyle = list(color = site_colors[["dark_gray"]], cursor = "default"),
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

change_map_bounds <- function(values) {
  extent <- max(abs(values), na.rm = TRUE)
  c(-extent, extent)
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
outline_legend_symbols <- JS(paste0("
  function() {
    this.legend.allItems.forEach(function(item) {
      if (item.legendSymbol) item.legendSymbol.attr({ stroke: '", site_colors[["dark_gray"]], "', 'stroke-width': 1 });
    });
  }"))

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
      value = spec$value, nullColor = site_colors[["grid"]],
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
### and labels each line's end with its name and label_format value in place
### of a legend
### value_label and value_format describe each point to screen readers
trend_chart <- function(data, value, title, subtitle = NULL, caption, value_label,
                        value_format = "{point.y:.1f}", label_format = value_format,
                        color = site_colors[["blue"]], compare = FALSE, expansion = 0.5,
                        floor = 0, ceiling = NULL, y_format = NULL, y_formatter = NULL) {
  bounds <- chart_bounds(data[[value]], expansion = expansion, floor = floor, ceiling = ceiling)
  accessibility <- list(point = list(valueDescriptionFormat = paste0(
    if (compare) "{point.series.name}, ", "year: {point.x:.0f}, ", value_label, ": ", value_format
  )))

  chart <- if (compare) {
    data$state_name <- forcats::fct_relevel(data$state_name, "United States", after = Inf)
    hchart(data, "spline", hcaes(year, !!rlang::sym(value), group = state_name),
           color = c(color, site_colors[["muted"]]), dashStyle = c("solid", "dash"),
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
      hc_chart(marginRight = end_label_margin, events = list(render = end_labels))
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
### color (default the series color, darkened only as far as needed for 4.5:1
### contrast on white)
### labels start at each line's latest point with a value; a value from before
### the chart's last year names its year
### close labels push apart, and the right margin widens to fit them
### the accessibility module already names each series, so labels are hidden
### from screen readers
end_labels <- JS("
  function() {
    var chart = this, lineHeight = 14, pad = 3;
    var narrow = chart.chartWidth < 500;
    var lastX = chart.xAxis[0].dataMax;
    var luminance = function(rgba) {
      var c = rgba.slice(0, 3).map(function(v) {
        v /= 255;
        return v <= 0.03928 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4);
      });
      return 0.2126 * c[0] + 0.7152 * c[1] + 0.0722 * c[2];
    };
    var readable = function(color) {
      for (var i = 0; i <= 10; i++) {
        var c = Highcharts.color(color).brighten(-i / 10);
        if (1.05 / (luminance(c.rgba) + 0.05) >= 4.5) return c.get('rgb');
      }
      return '#000000';
    };
    (chart.endLabels || []).forEach(function(label) { label.destroy(); });
    var items = chart.series.map(function(s) {
      var points = s.visible ? s.points.filter(function(p) { return !p.isNull; }) : [];
      if (!points.length) return null;
      var p = points.reduce(function(a, b) { return b.x > a.x ? b : a; });
      var custom = s.options.custom || {};
      var lines = [(narrow && custom.short) || custom.label || s.name];
      if (custom.value) {
        var value = Highcharts.format(custom.value, { point: p, series: s });
        lines.push(p.x < lastX ? value + ' (' + p.x + ')' : value);
      }
      var height = lines.length * lineHeight;
      var color = custom.color || readable(s.color);
      return { lines: lines, color: color, height: height, top: p.plotY - height / 2 };
    }).filter(Boolean).sort(function(a, b) { return a.top - b.top; });
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
        .attr({ 'aria-hidden': true })
        .add();
    });
    var width = Math.max.apply(null, chart.endLabels.map(function(label) { return label.getBBox().width; }).concat(0));
    var margin = Math.ceil(width) + 16;
    if (Math.abs(margin - (chart.options.chart.marginRight || 0)) > 1) {
      chart.update({ chart: { marginRight: margin } });
    }
  }")

### starting right margin for end labels, before end_labels measures them
end_label_margin <- 110

### short end labels for state comparison charts on narrow screens
state_short_names <- c(rlang::set_names(state.abb, state.name), "District of Columbia" = "DC")

### custom options end_labels reads for a state and U.S. comparison; value is
### the format string for each label's second line
### the U.S. reads "U.S." at every width, in a gray dark enough for text
compare_end_labels <- function(chart, value) {
  chart$x$hc_opts$series <- lapply(chart$x$hc_opts$series, function(series) {
    series$custom <- if (series$name == "United States") {
      list(label = "U.S.", value = value, color = site_colors[["muted"]])
    } else {
      list(short = unname(state_short_names[series$name]), value = value)
    }
    series
  })
  chart
}

### end labels for one line per offense, with a short name for aggravated
### assault on narrow charts; value is the format string for the second line
offense_end_labels <- function(chart, value) {
  chart$x$hc_opts$series <- lapply(chart$x$hc_opts$series, function(series) {
    series$custom <- list(value = value)
    if (series$name == "Aggravated assault") series$custom$short <- "Agg. assault"
    series
  })
  chart |>
    hc_legend(enabled = FALSE) |>
    hc_chart(marginRight = end_label_margin, events = list(render = end_labels))
}

### stacked columns of offense rates per 100k, one column per year
### data needs rate_label, the rate as tooltips show it, for screen readers
### colors follow the order of the group levels
offense_column_chart <- function(data, title, colors,
                                 caption = "FBI UCR Program, state crime estimates") {
  hchart(data, "column", hcaes(year, incidents_reported_rate_total, group = group),
         stacking = "normal", groupPadding = 0.05,
         accessibility = list(point = list(
           valueDescriptionFormat = "{point.series.name}, year: {point.x:.0f}, incidents per 100,000 residents: {point.rate_label}"
         ))) |>
    hc_colors(colors) |>
    hc_title(text = title) |>
    hc_subtitle(text = "Crime reported to police per 100,000 residents") |>
    hc_caption(text = caption) |>
    hc_legend(reversed = TRUE) |>
    hc_setup()
}

### reported rate trend for one offense in place, e.g., "Robbery in New York",
### subtitled "Robbery incidents reported to police per 100,000 residents" and
### drawn in the offense's color
### data needs rate_label, the rate as tooltips show it, for screen readers
offense_trend_chart <- function(data, offense, place,
                                caption = "FBI UCR Program, state crime estimates",
                                compare = FALSE) {
  trend_chart(
    dplyr::filter(data, group == offense), "incidents_reported_rate_total",
    paste(offense, "in", place),
    paste(offense, "incidents reported to police per 100,000 residents"), caption,
    value_label = paste(tolower(offense), "incidents per 100,000 residents"),
    value_format = "{point.rate_label}",
    color = offense_color(offense), compare = compare
  )
}

### backlog charts styled like the Crime Without Consequence microsite: larger
### type and bordered cards, in the site colors and GT America
### hc_add_theme() replaces the theme hc_setup() adds, so this builds on hc_theme_jc
hc_theme_microsite <- hc_theme_merge(
  hc_theme_jc,
  hc_theme(
    chart = list(borderColor = site_colors[["border"]], borderWidth = 1, borderRadius = 8),
    title = list(style = list(fontSize = "22px", fontWeight = "700")),
    subtitle = list(style = list(fontSize = "15px")),
    caption = list(style = list(color = site_colors[["muted"]], fontSize = "13px")),
    xAxis = list(labels = list(style = list(fontSize = "13px"))),
    yAxis = list(labels = list(style = list(fontSize = "13px"))),
    legend = list(itemStyle = list(color = site_colors[["text"]], fontSize = "14px", fontWeight = "600")),
    tooltip = list(backgroundColor = "#FFFFFF", borderColor = site_colors[["border"]],
                   borderRadius = 8, style = list(color = site_colors[["text"]], fontSize = "14px"))
  )
)

### sets the chart's long description for screen readers and keeps the other
### accessibility options hc_setup() set
hc_describe <- function(chart, text) {
  chart$x$hc_opts$accessibility$description <- text
  chart
}

### compact JS number format, e.g., 1.31M, 563K, 25.3K or 840
### trim drops trailing zeros and dm sets the decimals for millions
compact_number_js <- "var fmt = function(v, trim, dm) {
    var a = Math.abs(v), out;
    dm = (dm === undefined) ? 2 : dm;
    if (a >= 1e6) { out = (v / 1e6).toFixed(dm); return (trim ? String(+out) : out) + 'M'; }
    if (a >= 1e3) { out = (v / 1e3).toFixed(a >= 1e5 ? 0 : 1); return (trim ? String(+out) : out) + 'K'; }
    return String(Math.round(v));
  };"

### y-axis labels in compact numbers; plus labels read +1M, +2M, with a blue
### "?" at the baseline because the backlog from earlier years is unknown
compact_axis_labels <- function(plus = FALSE) {
  zero <- if (plus) paste0("'<span style=\"color:", site_colors[["blue"]], ";font-weight:900\">?</span>'") else "'0'"
  value <- if (plus) "'+' + fmt(v, true)" else "fmt(v, true)"
  JS(paste0("function() { ", compact_number_js, "
    var v = this.value;
    if (v === 0) return ", zero, ";
    return ", value, ";
  }"))
}

compact_point_label <- JS(paste0("function() { ", compact_number_js, " return fmt(this.y, false); }"))

### the series name at its last point with a value
last_point_name_label <- JS("function() {
  var points = this.series.points.filter(function(p) { return p.y !== null; });
  return this.point === points[points.length - 1] ? this.series.name : null;
}")

### the last point's value with a plus sign, e.g., +4.3M
backlog_end_label <- JS(paste0("function() { ", compact_number_js, "
  return this.point.index === this.series.data.length - 1 ? '+' + fmt(this.y, true, 1) : null;
}"))

### render event that draws a note and a dotted line down to the baseline,
### saying the backlog adds to unsolved crime from before first_year
### note_frac places the note's center as a share of the y-axis range, and
### end_frac ends the line that share of the plot height above the baseline
### the note's box is saved so backlog_tooltip_position() can steer around it
backlog_note <- function(first_year, note_frac = 0.45, max_width = 170, end_frac = 0.09) {
  JS(paste0("function() {
  var chart = this, accent = '", site_colors[["blue"]], "', axis = chart.yAxis[0];
  if (chart.noteGroup) chart.noteGroup.destroy();
  var g = chart.renderer.g('not-starting-at-zero-note').attr({ zIndex: 7 }).add();
  chart.noteGroup = g;
  var xEnd = chart.plotLeft + 2;
  var yEnd = chart.plotTop + chart.plotHeight * (1 - ", end_frac, ");
  var yCenter = axis.toPixels(axis.min + ", note_frac, " * (axis.max - axis.min));
  var html = '<span style=\"color:' + accent + ';font-weight:700\">Not starting at zero:</span> ' +
             'added on top of unsolved crimes from before ", first_year, "';
  var note = chart.renderer.label(html, xEnd + 6, 0, 'rect')
    .css({ width: Math.min(", max_width, ", chart.plotWidth * 0.3) + 'px', color: '#000000', fontSize: '13px' })
    .attr({ fill: 'none', padding: 8, zIndex: 8 })
    .add(g);
  var box = note.getBBox();
  note.attr({ y: yCenter - box.height / 2 });
  chart.renderer.path(['M', xEnd + 22, yCenter + box.height / 2, 'L', xEnd, yEnd])
    .attr({ stroke: accent, 'stroke-width': 1.5, 'stroke-dasharray': '3,3', fill: 'none', zIndex: 7 })
    .add(g);
  chart.noteBox = { left: xEnd + 6, right: xEnd + 6 + box.width,
                    top: yCenter - box.height / 2, bottom: yCenter + box.height / 2 };
}"))
}

### tooltips sit beside the point inside the plot, and drop below the note
### when they would cover it
backlog_tooltip_position <- JS("function(labelWidth, labelHeight, point) {
  var chart = this.chart, gap = 12;
  var plotBottom = chart.plotTop + chart.plotHeight;
  var px = chart.plotLeft + point.plotX, py = chart.plotTop + point.plotY;
  var x = px + gap;
  if (x + labelWidth > chart.plotLeft + chart.plotWidth) x = px - labelWidth - gap;
  var y = Math.min(Math.max(py - labelHeight / 2, chart.plotTop), plotBottom - labelHeight);
  var nb = chart.noteBox;
  if (nb && x < nb.right && x + labelWidth > nb.left && y < nb.bottom && y + labelHeight > nb.top) {
    y = Math.min(nb.bottom + 6, plotBottom - labelHeight);
  }
  return { x: x, y: y };
}")

### plural offense names for backlog text
backlog_offense_plurals <- c(
  "Aggravated assault" = "Aggravated assaults", "Robbery" = "Robberies",
  "Rape" = "Rapes", "Homicide" = "Homicides"
)

### each offense's share of the latest year's backlog, largest first
### offenses is add_backlog() output by offense
backlog_shares <- function(offenses) {
  offenses |>
    dplyr::filter(year == max(year)) |>
    dplyr::mutate(group = as.character(group), share = backlog / sum(backlog)) |>
    dplyr::arrange(dplyr::desc(share)) |>
    dplyr::select(group, share)
}

### heading naming the offense with the largest share of the backlog, e.g.,
### "Aggravated assaults make up 65% of the backlog."
backlog_headline <- function(offenses) {
  top <- dplyr::filter(backlog_shares(offenses), group %in% names(backlog_offense_plurals))[1, ]
  if (!is.finite(top$share)) stop("The backlog has no unsolved crime to divide by offense", call. = FALSE)
  paste0(backlog_offense_plurals[[top$group]], " make up ", percent(top$share), " of the backlog.")
}

### tooltip counts, with "not available" for years without a solve rate
backlog_count <- function(x) dplyr::if_else(is.na(x), "not available", comma(x))

### caption note naming the solve rates the backlog leaves out, e.g., "Solve
### rates are not shown for all offenses in 2021 or for homicide and rape in
### 2022, so ...", and the years an unsplit slice holds offenses together, or ""
### offenses and totals are add_backlog() output
backlog_missing_note <- function(offenses, totals) {
  unsplit <- if ("unsplit" %in% names(offenses)) offenses$unsplit %in% TRUE else rep(FALSE, nrow(offenses))
  unsplit_years <- offenses$year[unsplit & !is.na(offenses$incidents_unsolved)]
  offenses <- offenses[!unsplit, ]
  n_offenses <- dplyr::n_distinct(offenses$group)
  hidden <- offenses |>
    dplyr::filter(is.na(incidents_unsolved)) |>
    dplyr::summarize(groups = list(sort(tolower(as.character(group)))), .by = year)
  ### a missing total with every offense shown hides the whole year
  whole_years <- sort(union(hidden$year[lengths(hidden$groups) == n_offenses],
                            setdiff(totals$year[is.na(totals$incidents_unsolved)], hidden$year)))
  partial <- dplyr::filter(hidden, lengths(groups) < n_offenses, !year %in% whole_years)
  combined <- dplyr::filter(partial, year %in% unsplit_years)
  partial <- dplyr::filter(partial, !year %in% unsplit_years)

  pieces <- c(
    if (length(whole_years) > 0) paste("all offenses in", knitr::combine_words(whole_years)),
    vapply(seq_len(nrow(partial)), \(i) {
      paste(knitr::combine_words(partial$groups[[i]]), "in", partial$year[i])
    }, character(1))
  )
  missing <- if (length(pieces) > 0) {
    paste0("Solve rates are not shown for ",
           knitr::combine_words(pieces, sep = ", for ", and = " or for ", oxford_comma = FALSE),
           ", so the backlog leaves out those crimes and is a minimum.")
  }
  together <- vapply(seq_len(nrow(combined)), \(i) {
    groups <- combined$groups[[i]]
    paste0("In ", combined$year[i], ", ", knitr::combine_words(groups),
           if (length(groups) > 1) " are" else " is", " counted together, not broken out, because ",
           if (length(groups) > 1) "their separate solve rates are" else "its own solve rate is", " not shown.")
  }, character(1))
  paste(c(missing, together), collapse = " ")
}

### a reported, solved and unsolved trend and a backlog stacked by offense
### totals and offenses are add_backlog() output; place reads in titles, e.g.,
### "Ohio"; y_expansion is headroom above the largest value and x_pad the space
### in years beside the first and last year
backlog_charts <- function(totals, offenses, place,
                           caption = "FBI UCR Program, state crime estimates and clearances",
                           y_expansion = 0.2, x_pad = 0.4) {
  years <- totals$year
  first_year <- min(years)
  last_year <- max(years)
  ### unsplit slices (see add_backlog()) stack on top of the offenses
  unsplit_groups <- unique(as.character(offenses$group[offenses$unsplit %in% TRUE]))
  slice_colors <- c(offense_colors, setNames(rep(backlog_colors[["unsplit"]], length(unsplit_groups)), unsplit_groups))
  offense_order <- names(slice_colors)
  offenses <- offenses |>
    dplyr::filter(group %in% offense_order) |>
    dplyr::mutate(group = factor(group, levels = offense_order))
  if (nrow(offenses) != length(offense_order) * length(years)) {
    stop("Backlog charts need every offense in every year for ", place, call. = FALSE)
  }

  ### the slices must add up to the total line, or the chart contradicts itself;
  ### state totals are sums of their offenses, and pooled US offense rates differ
  ### from the pooled total by well under the tolerance
  slice_check <- offenses |>
    dplyr::summarize(slices = sum(backlog), .by = year) |>
    dplyr::inner_join(dplyr::select(totals, year, backlog), by = "year")
  if (any(abs(slice_check$slices - slice_check$backlog) / pmax(slice_check$backlog, 1) > 0.005)) {
    stop("Offense slices do not add up to the total backlog for ", place,
         ". Build state totals with backlog_totals().", call. = FALSE)
  }

  missing_note <- backlog_missing_note(offenses, totals)
  caption <- paste(c(if (missing_note != "") missing_note, caption), collapse = "<br>")

  trend_data <- totals |>
    dplyr::mutate(tooltip = paste0(
      "<b>", year, "</b><br>",
      "Violent crimes reported: ", backlog_count(incidents_reported), "<br>",
      "Violent crimes solved: ", backlog_count(incidents_solved), "<br>",
      "Violent crimes unsolved: ", backlog_count(incidents_unsolved), "<br>",
      "<b>Total unsolved since ", first_year, ": ", comma(backlog), "</b>"
    ))

  ### each offense's unsolved count that year, top to bottom like the slices,
  ### with a matching swatch; in a year with an unsplit count, its offenses
  ### show only in that line
  offense_tooltip <- offenses |>
    dplyr::mutate(unsplit_year = any(group %in% unsplit_groups & !is.na(incidents_unsolved)), .by = year) |>
    dplyr::filter(!is.na(incidents_unsolved) | !(unsplit_year | group %in% unsplit_groups)) |>
    dplyr::arrange(year, dplyr::desc(group)) |>
    dplyr::summarize(
      offense_text = paste0(
        "<span style='color:", slice_colors[as.character(group)], "'>●</span> ",
        group, ": ", backlog_count(incidents_unsolved), collapse = "<br>"
      ),
      .by = year
    )

  backlog_data <- totals |>
    dplyr::left_join(offense_tooltip, by = "year") |>
    dplyr::mutate(tooltip = paste0(
      "<b>", year, "</b><br>",
      "Unsolved this year: ", backlog_count(incidents_unsolved), "<br>",
      offense_text, "<br>",
      "<b>Total added since ", first_year, ": ", comma(backlog), "</b>"
    ))

  shown <- years[!is.na(totals$incidents_solved)]
  if (length(shown) == 0) {
    stop("No year has a solve rate for ", place,
         ". Add it to states_to_exclude_from_solved_rate_viz.", call. = FALSE)
  }

  ### "Unsolved" sits halfway between the lines at the middle year, or at the
  ### nearest year with a solve rate when the middle falls in a gap
  label_x <- mean(range(years))
  if (!all(c(floor(label_x), ceiling(label_x)) %in% shown)) label_x <- shown[which.min(abs(shown - label_x))]
  mid <- (totals$incidents_reported + totals$incidents_solved) / 2
  gap_label_data <- tibble::tibble(
    year = label_x,
    mid = if (label_x %in% years) mid[years == label_x] else stats::approx(years, mid, xout = label_x)$y
  )

  shown_totals <- dplyr::filter(totals, !is.na(incidents_solved))
  solved_text <- if (nrow(shown_totals) == 1) {
    paste0("Violent crimes solved were ", comma(shown_totals$incidents_solved), " in ", shown_totals$year)
  } else {
    paste0("Violent crimes solved ranged from ", comma(min(shown_totals$incidents_solved)), " to ",
           comma(max(shown_totals$incidents_solved)), " a year")
  }
  desc_trend <- paste0(
    "Line chart with a shaded gap, ", first_year, " to ", last_year, ", for ", place,
    ". Violent crimes reported went from ", comma(dplyr::first(totals$incidents_reported)), " in ",
    first_year, " to ", comma(dplyr::last(totals$incidents_reported)), " in ", last_year,
    ". ", solved_text, ". The shaded area between the two lines ",
    "is the number left unsolved each year. In total, ", comma(dplyr::last(totals$backlog)),
    " violent crimes went unsolved from ", first_year, " through ", last_year, ".",
    if (missing_note != "") paste0(" ", missing_note)
  )

  shares <- backlog_shares(offenses)
  desc_backlog <- paste0(
    "Stacked area chart, ", first_year, " to ", last_year, ", for ", place,
    ". The backlog of unsolved violent crimes added since ", first_year, " reaches ",
    comma(dplyr::last(totals$backlog)), " by the end of ", last_year,
    ". The area is divided by offense. By the end of ", last_year, ", the backlog is ",
    knitr::combine_words(paste(sub(", not broken out$", " combined", tolower(shares$group)), percent(shares$share, suffix = " percent"))),
    ". The chart does not start from zero in real terms: these crimes are added on top of ",
    "an unknown number of unsolved crimes from before ", first_year, ".",
    if (missing_note != "") paste0(" ", missing_note)
  )

  ### value labels above or below each point, and the series name at the line's end
  line_labels <- function(color, above) {
    list(
      list(enabled = TRUE, formatter = compact_point_label,
           verticalAlign = if (above) "bottom" else "top", y = if (above) -6 else 8,
           style = list(color = color, fontSize = "13px", fontWeight = "400", textOutline = "none")),
      list(enabled = TRUE, formatter = last_point_name_label,
           align = "left", verticalAlign = "middle", x = 10, y = 0,
           crop = FALSE, overflow = "allow", allowOverlap = TRUE,
           style = list(color = color, fontSize = "15px", fontWeight = "700", textOutline = "none"))
    )
  }
  point_format <- function(label) list(valueDescriptionFormat = paste0("year: {point.x:.0f}, ", label, ": {point.y:,.0f}"))
  trend_bounds <- chart_bounds(c(0, totals$incidents_reported), expansion = y_expansion, floor = 0)

  ### the shaded gap stacks the unsolved count on an invisible series that
  ### follows the solved line
  trend <- hchart(
    trend_data, "area", hcaes(year, incidents_solved),
    name = "Solved (base of gap)", color = "transparent", fillOpacity = 0,
    lineWidth = 0, stacking = "normal", marker = list(enabled = FALSE),
    enableMouseTracking = FALSE, zIndex = 0, accessibility = list(enabled = FALSE)
  ) |>
    hc_add_series(
      trend_data, "area", hcaes(year, incidents_unsolved),
      name = "Unsolved", color = backlog_colors[["unsolved"]], fillOpacity = 1, lineWidth = 0,
      stacking = "normal", marker = list(enabled = FALSE), enableMouseTracking = FALSE, zIndex = 0,
      accessibility = list(description = "Shaded area showing violent crimes left unsolved each year.",
                           point = point_format("violent crimes unsolved"))
    ) |>
    hc_add_series(
      gap_label_data, "line", hcaes(year, mid),
      name = "Unsolved (label)", lineWidth = 0, marker = list(enabled = FALSE),
      enableMouseTracking = FALSE, zIndex = 1,
      dataLabels = list(enabled = TRUE, format = "Unsolved", align = "center",
                        verticalAlign = "middle", y = 0, allowOverlap = TRUE,
                        style = list(color = "#000000", fontSize = "15px",
                                     fontWeight = "700", textOutline = "none")),
      accessibility = list(enabled = FALSE)
    ) |>
    hc_add_series(
      trend_data, "line", hcaes(year, incidents_reported),
      name = "Reported", color = backlog_colors[["reported"]], lineWidth = 3, zIndex = 2,
      marker = list(symbol = "circle", radius = 4, lineColor = "#FFFFFF", lineWidth = 1.5),
      dataLabels = line_labels(backlog_colors[["reported"]], above = TRUE),
      accessibility = list(description = "Violent crimes reported each year.",
                           point = point_format("violent crimes reported"))
    ) |>
    ### dashes and diamonds set solved apart by more than color
    hc_add_series(
      trend_data, "line", hcaes(year, incidents_solved),
      name = "Solved", color = backlog_colors[["solved"]], lineWidth = 3, dashStyle = "ShortDash", zIndex = 2,
      marker = list(symbol = "diamond", radius = 5, lineColor = "#FFFFFF", lineWidth = 1.5),
      dataLabels = line_labels(backlog_colors[["solved"]], above = FALSE),
      accessibility = list(description = "Violent crimes solved each year.",
                           point = point_format("violent crimes solved"))
    ) |>
    hc_title(text = paste("Violent crime reported, solved, and unsolved in", place)) |>
    hc_caption(text = caption) |>
    hc_legend(enabled = FALSE) |>
    hc_setup() |>
    hc_add_theme(hc_theme_microsite) |>
    ### reversedStacks = FALSE keeps the invisible base at the bottom
    hc_xAxis(tickPositions = years, min = first_year - x_pad, max = last_year + x_pad) |>
    hc_yAxis(min = trend_bounds[1], max = trend_bounds[2], endOnTick = FALSE, reversedStacks = FALSE,
             labels = list(formatter = compact_axis_labels())) |>
    hc_chart(marginRight = 70) |>
    hc_describe(desc_trend)

  ### one slice per offense, with the slices' numbers in the total line's
  ### tooltip and, for screen readers, in each slice's descriptions
  backlog <- highchart()
  for (g in offense_order) {
    backlog <- backlog |>
      hc_add_series(
        dplyr::filter(offenses, group == g), "area", hcaes(year, backlog),
        name = g, color = slice_colors[[g]], fillOpacity = 1,
        lineWidth = 1, lineColor = "#FFFFFF", stacking = "normal",
        marker = list(enabled = FALSE, states = list(hover = list(enabled = FALSE))),
        enableMouseTracking = FALSE, zIndex = 1,
        accessibility = list(
          description = paste0(g, ": unsolved cases added to the backlog since ", first_year, "."),
          point = list(valueDescriptionFormat = paste0(
            "{point.series.name}, year: {point.x:.0f}, unsolved added since ", first_year, ": {point.y:,.0f}"))
        )
      )
  }

  backlog_bounds <- chart_bounds(c(0, totals$backlog), expansion = y_expansion, floor = 0)
  backlog <- backlog |>
    ### the total line is left out of the legend because the slices add up to it
    hc_add_series(
      backlog_data, "line", hcaes(year, backlog),
      name = paste("Total backlog added since", first_year),
      color = backlog_colors[["reported"]], lineWidth = 2, zIndex = 3, showInLegend = FALSE,
      marker = list(enabled = TRUE, symbol = "circle", radius = 4,
                    fillColor = "#000000", lineColor = "#FFFFFF", lineWidth = 1.5),
      dataLabels = list(enabled = TRUE, align = "right", verticalAlign = "bottom", y = -8,
                        formatter = backlog_end_label,
                        style = list(color = "#000000", fontSize = "13px",
                                     fontWeight = "700", textOutline = "none")),
      accessibility = list(
        description = paste0("Total unsolved violent crimes added to the backlog since ",
                             first_year, ", all offenses combined."),
        point = point_format(paste("total unsolved violent crimes added since", first_year))
      )
    ) |>
    hc_title(text = paste("Backlog of unsolved violent crime in", place)) |>
    hc_subtitle(text = paste0("Unsolved violent crimes added since ", first_year, ", by offense")) |>
    hc_caption(text = caption) |>
    hc_legend(enabled = TRUE, reversed = FALSE) |>
    hc_setup() |>
    hc_add_theme(hc_theme_microsite) |>
    hc_xAxis(tickPositions = years, min = first_year - x_pad * 0.75, max = last_year + x_pad * 0.75) |>
    ### reversedStacks = FALSE puts aggravated assault at the bottom
    hc_yAxis(min = backlog_bounds[1], max = backlog_bounds[2], endOnTick = FALSE, reversedStacks = FALSE,
             labels = list(formatter = compact_axis_labels(plus = TRUE), useHTML = TRUE)) |>
    hc_chart(events = list(render = backlog_note(first_year))) |>
    hc_tooltip(positioner = backlog_tooltip_position) |>
    hc_describe(desc_backlog)

  list(trend = trend, backlog = backlog)
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
shr_series_colors <- unname(site_colors[c("blue", "muted")])

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
solve_rate_colors <- c(above = site_colors[["dark_blue"]], below = site_colors[["orange"]])
solve_rate_fills <- c(above = site_colors[["white"]], below = site_colors[["light_orange"]])

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
  border <- list(borderRight = paste("1px solid", site_colors[["grid"]]))
  colDef(
    name = name, footer = footer, minWidth = table_widths$name, sticky = "left",
    style = border, headerStyle = border, footerStyle = border, ...
  )
}

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
