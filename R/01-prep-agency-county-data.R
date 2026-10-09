# Build the county map and agency table data for the state pages from the
# CDE agency file. Run once before rendering the 51 state pages.

library(tidyverse)
library(csgjcr)

source("R/local-map-data.R")
source("R/agency-county-data.R")
source("R/page-data.R")

### base_year comes from R/page-data.R
agency_year <- 2025

raw <- read_rds(csg_sp_path("jr_data_library", "data", "analysis", "fbi", "srs", "fbi_srs_agency.rds"))

agency_county_data <- build_agency_county_data(raw, agency_year, base_year)

dir.create("data", showWarnings = FALSE)
iwalk(agency_county_data, \(x, name) write_rds(x, file.path("data", paste0(name, ".rds"))))
