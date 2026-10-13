# Refresh data/nypd_clearance_2022.csv, NYPD's 2022 citywide clearance rates.
# NYPD filed offenses but no clearances with the FBI in 2022, so the site takes
# its clearance rates from NYPD's quarterly Local Law 42 reports instead.
# The reports give rates only, so the annual rate averages the four quarters.

year <- 2022
ori <- "NY0303000"
destination <- "data/nypd_clearance_2022.csv"
report_url <- "https://www.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/clearance/clearance-report-q%d-%d.xlsx"

# report row labels for each site offense
offenses <- data.frame(
  group = c("Homicide", "Rape", "Robbery", "Aggravated assault"),
  report_label = c("Murder", "Expanded Rape", "Robbery", "Aggravated Assault")
)

# citywide share cleared for each offense in one quarterly report
read_report_quarter <- function(quarter) {
  url <- sprintf(report_url, quarter, year)
  path <- tempfile(fileext = ".xlsx")
  tryCatch(
    # nyc.gov refuses R's default user agent
    download.file(url, path, mode = "wb", quiet = TRUE, headers = c("User-Agent" = "Mozilla/5.0")),
    error = function(e) stop("Could not download ", url, ": ", conditionMessage(e))
  )
  on.exit(unlink(path))

  # nyc.gov can answer a bad link with an HTML page
  x <- tryCatch(
    readxl::read_excel(path, col_names = FALSE, col_types = "text", .name_repair = "unique_quiet"),
    error = function(e) stop(url, " is not an Excel report: ", conditionMessage(e))
  )

  title <- sprintf("%s Quarter - CY %d", c("1st", "2nd", "3rd", "4th")[quarter], year)
  if (!title %in% trimws(x[[1]])) stop(url, " does not say ", title)

  header_row <- which(apply(x, 1, function(r) any(trimws(r) == "Citywide", na.rm = TRUE)))[1]
  if (is.na(header_row)) stop("Unexpected layout in ", url)
  citywide <- x[[which(trimws(unlist(x[header_row, ])) == "Citywide")]]
  rates <- as.numeric(citywide[match(offenses$report_label, trimws(x[[1]]))])
  if (anyNA(rates)) stop("Unexpected layout in ", url)

  # clearances can include crimes from earlier quarters, so rates can top 1
  if (any(rates < 0 | rates > 1.5)) stop("Citywide rates out of range in ", url)

  rates
}

quarters <- sapply(1:4, read_report_quarter)

clearance <- data.frame(
  year = year,
  ori = ori,
  group = offenses$group,
  clearance_rate = rowMeans(quarters)
)

write.csv(clearance, destination, row.names = FALSE)
message("Saved ", destination)
