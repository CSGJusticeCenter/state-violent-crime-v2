# Build the county map and agency table data for the state pages from the
# CDE agency file. Run once before rendering the 51 state pages.

library(tidyverse)
library(csgjcr)

source("R/local-map-data.R")
source("R/agency-county-data.R")

agency_year <- 2025
base_year <- 2019

raw <- read_rds(csg_sp_path("jr_data_library", "data", "analysis", "fbi", "srs", "fbi_srs_agency.rds"))

agency_county_data <- build_agency_county_data(raw, agency_year, base_year)

dir.create("data", showWarnings = FALSE)
iwalk(agency_county_data, \(x, name) write_rds(x, file.path("data", paste0(name, ".rds"))))

# CSG regions used by the supplemental homicide section.
file.copy(
  csg_sp_path("z_ARCHIVE", "Ad_Hoc_Requests", "state_violent_crime_marshall", "data", "csg-regions.csv"),
  "data/csg-regions.csv",
  overwrite = TRUE
)
