# Read a vendored Highcharts county map without making a network request.
read_local_county_map <- function(map, root = "maps") {
  path <- file.path(root, map)
  if (!file.exists(path)) {
    stop("Missing local Highcharts county map: ", path, call. = FALSE)
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines[1] <- sub("^.* = ", "", lines[1])
  json <- sub(";$", "", paste(lines, collapse = "\n"))
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

# Build the same map series as hcmap(), using repo data instead of its CDN fetch.
hcmap_local <- function(map, data, value, joinBy, ..., root = "maps") {
  map <- paste0(sub("\\.js$", "", map), ".js")
  map_data <- read_local_county_map(map, root)
  data <- dplyr::rename(data, value = !!rlang::sym(value))

  highcharter::highchart(type = "map") |>
    highcharter::hc_add_series(
      mapData = map_data,
      data = highcharter::list_parse(data),
      joinBy = joinBy,
      ...
    ) |>
    highcharter::hc_colorAxis(auxpar = NULL) |>
    highcharter::hc_credits(enabled = TRUE)
}
