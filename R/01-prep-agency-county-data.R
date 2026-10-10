# Build the county map and agency table data for the state pages, and state
# reporting coverage for the national methodology, from the CDE agency file.
# Run once before rendering the site.

library(tidyverse)
library(csgjcr)

### base_year and violent_offenses come from R/page-data.R
source("R/page-data.R")
source("R/local-map-data.R")
source("R/agency-county-data.R")

agency_year <- 2025

raw <- read_rds(csg_sp_path("jr_data_library", "data", "analysis", "fbi", "srs", "fbi_srs_agency.rds"))

pep <- read_rds(csg_sp_path("jr_data_library", "data", "analysis", "census", "pep", "census_pep_state.rds")) |>
  filter(group == "Total", year == agency_year) |>
  distinct(state_abbr, state_name, pop_total)

agency_county_data <- build_agency_county_data(raw, agency_year, base_year)
agency_county_data$state_coverage <- state_reporting_coverage(raw, pep, agency_year)

dir.create("data", showWarnings = FALSE)
iwalk(agency_county_data, \(x, name) write_rds(x, file.path("data", paste0(name, ".rds"))))
