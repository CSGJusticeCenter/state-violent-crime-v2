# Refresh data/ucr_coverage_2021.csv, the share of each state's population
# covered by agencies reporting violent crime to the UCR in 2021.
# Source: FBI Crime Data Explorer API, summarized endpoint, NIBRS and SRS.
# Needs a data.gov key in DATA_GOV_API_KEY.

source("R/cde-api.R")

year <- 2021
geos <- c("national", state.abb, "DC")
destination <- "data/ucr_coverage_2021.csv"

# population repeats the annual figure each month, so the mean is that figure
annual_mean <- function(months) mean(unlist(months), na.rm = TRUE)

coverage <- lapply(geos, function(geo) {
  path <- if (geo == "national") "national" else paste0("state/", geo)
  js <- read_cde_summarized(path, "violent-crime", year)

  # population lists hold a branch for the state and one for the nation
  # the offense series names which branch is this geography's
  offense_key <- grep(" Offenses$", names(js$offenses$actuals), value = TRUE)
  geo_name <- sub(" Offenses$", "", offense_key)
  pops <- js$populations
  if (length(geo_name) != 1 ||
      is.null(pops$population[[geo_name]]) ||
      is.null(pops$participated_population[[geo_name]])) {
    stop("Unexpected CDE population data for ", geo)
  }

  data.frame(
    state_name = geo_name,
    population = annual_mean(pops$population[[geo_name]]),
    participated_population = annual_mean(pops$participated_population[[geo_name]])
  )
})

coverage <- do.call(rbind, coverage)
coverage$coverage <- coverage$participated_population / coverage$population

if (nrow(coverage) != 52 || anyNA(coverage$coverage)) {
  stop("Expected coverage for 50 states, DC and the US")
}

write.csv(coverage, destination, row.names = FALSE)
message("Saved ", destination)
