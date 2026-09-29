# Refresh the 51 county maps used by state-viol-crime.qmd.
# Source: Highcharts Map Collection, pinned to v2.3.3.
version <- "v2.3.3"
states <- tolower(c(state.abb, "DC"))

for (state in states) {
  relative_path <- sprintf("countries/us/us-%s-all.js", state)
  destination <- file.path("maps", relative_path)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  url <- sprintf(
    "https://raw.githubusercontent.com/highcharts/map-collection-dist/%s/%s",
    version, relative_path
  )

  temporary_file <- tempfile(fileext = ".js")
  tryCatch(
    download.file(url, temporary_file, mode = "wb", quiet = TRUE),
    error = function(e) stop("Could not download ", url, ": ", conditionMessage(e))
  )
  first_line <- readLines(temporary_file, n = 1, warn = FALSE)
  if (length(first_line) != 1L ||
      !startsWith(first_line, sprintf('Highcharts.maps["countries/us/us-%s-all"]', state))) {
    stop("Unexpected Highcharts map data from ", url)
  }
  if (!file.copy(temporary_file, destination, overwrite = TRUE)) {
    stop("Could not save ", destination)
  }
  unlink(temporary_file)
  message("Saved ", destination)
}
