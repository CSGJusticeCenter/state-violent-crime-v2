# FIPS codes for the five NYC boroughs, and the key for the merged NYC feature.
nyc_borough_fips <- c("36005", "36047", "36061", "36081", "36085")
nyc_fips <- "36NYC"

# Convert a GeoJSON ring (list of coordinate pairs) to a coordinate matrix.
ring_to_matrix <- function(ring) {
  do.call(rbind, lapply(ring, unlist))
}

# Convert a parsed GeoJSON Polygon or MultiPolygon to an sfg object.
geometry_to_sfg <- function(geometry) {
  if (geometry$type == "Polygon") {
    sf::st_polygon(lapply(geometry$coordinates, ring_to_matrix))
  } else {
    sf::st_multipolygon(lapply(geometry$coordinates, function(polygon) {
      lapply(polygon, ring_to_matrix)
    }))
  }
}

# Convert an sfg Polygon or MultiPolygon back to a parsed GeoJSON geometry.
sfg_to_geometry <- function(sfg) {
  matrix_to_ring <- function(m) lapply(seq_len(nrow(m)), function(i) c(m[i, 1], m[i, 2]))
  polygon_to_rings <- function(rings) lapply(rings, matrix_to_ring)

  if (inherits(sfg, "MULTIPOLYGON")) {
    list(type = "MultiPolygon", coordinates = lapply(unclass(sfg), polygon_to_rings))
  } else {
    list(type = "Polygon", coordinates = polygon_to_rings(unclass(sfg)))
  }
}

# Replace the five NYC borough features with one NYC feature.
# Map coordinates are already projected, so the union runs without a CRS.
# Maps that lack any of the five boroughs are returned unchanged.
merge_nyc_boroughs <- function(map) {
  fips <- vapply(map$features, function(feature) feature$properties$fips, character(1))
  is_borough <- fips %in% nyc_borough_fips
  if (sum(is_borough) != length(nyc_borough_fips)) {
    return(map)
  }

  boroughs <- map$features[is_borough]
  union <- sf::st_union(sf::st_sfc(lapply(lapply(boroughs, `[[`, "geometry"), geometry_to_sfg)))

  merged <- list(
    type = "Feature",
    id = "US.NY.NYC",
    properties = list(
      `hc-group` = "admin2",
      `hc-key` = "us-ny-nyc",
      `hc-a2` = "NY",
      fips = nyc_fips,
      name = "New York City"
    ),
    geometry = sfg_to_geometry(union[[1]])
  )

  map$features <- c(map$features[!is_borough], list(merged))
  map
}

# Read a vendored Highcharts county map without making a network request.
read_local_county_map <- function(map, root = "maps") {
  path <- file.path(root, map)
  if (!file.exists(path)) {
    stop("Missing local Highcharts county map: ", path, call. = FALSE)
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines[1] <- sub("^.* = ", "", lines[1])
  json <- sub(";$", "", paste(lines, collapse = "\n"))
  merge_nyc_boroughs(jsonlite::fromJSON(json, simplifyVector = FALSE))
}

# List the county features of a vendored map as fips and name.
local_county_features <- function(map, root = "maps") {
  map <- paste0(sub("[.]js$", "", map), ".js")
  features <- read_local_county_map(map, root)$features
  data.frame(
    county_fips = vapply(features, function(f) f$properties$fips, character(1)),
    county_label = vapply(features, function(f) f$properties$name, character(1))
  )
}

# Build the same map series as hcmap(), using repo data instead of its CDN fetch.
hcmap_local <- function(map, data, value, joinBy, ..., root = "maps") {
  map <- paste0(sub("[.]js$", "", map), ".js")
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
