## get data about agency nibrs status and population from ucr/fbi api
## also pull state-level nibrs participation data from fbi api
## api docs: https://crime-data-explorer.fr.cloud.gov/pages/docApi
## note you'll need a data.gov api key - request here: https://api.data.gov/signup/
## after you get your key, store as an r environment variable names "DATA_GOV_API_KEY"

library(tidyverse)
library(httr2)
library(lubridate)
library(csgjcr)

# path to data folder on sharepoint
sp_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")

# define function to pull state nibrs participation data from fbi api cde
# for a given state, this will return state and population percent coverage
get_state_nibrs_cov <- function(state) {
  message("Getting ", state, " NIBRS participation info")
  request("https://api.usa.gov/crime/fbi/sapi/api/participation/states/") |>
    req_url_path_append(state) %>%
    req_url_query(API_KEY = Sys.getenv("DATA_GOV_API_KEY")) |>
    req_headers(Accept = "application/json") |>
    req_retry(max_tries = 5) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE) |>
    pluck("results") |>
    as_tibble() |>
    filter(data_year == max(data_year)) |>
    transmute(
      state_abb = state_abbr,
      pop_cov = nibrs_population_percentage_covered / 100
    )
}

# for each state, pull nibrs participation and coverage
state_nibrs_pop_cov <- map_dfr(c(state.abb, "DC"), get_state_nibrs_cov)

# define function to get agency population from fbi cde api
# can only pull one agency (ori) at a time
# we'll use this for agencies where the population was missing from offenses known
get_agency_pop <- function(ori) {
  message("Getting ", ori, " agency info")
  request("https://api.usa.gov/crime/fbi/sapi/api/participation/agencies/") |>
    req_url_path_append(ori) %>%
    req_url_query(API_KEY = Sys.getenv("DATA_GOV_API_KEY")) |>
    req_headers(Accept = "application/json") |>
    req_retry(max_tries = 5) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE) |>
    pluck("results") |>
    as_tibble()
}


# read in ucr srs offenses known
# we just need this to get population info for agencies, not the actual crime data
okca <- read_rds(file.path(sp_path, "offenses_known_yearly_1960_2020.rds"))

# make api request to fbi cde api for all ucr reporting agencies
# contains agency name, state, county and nibrs status
all_ucr_agency <- request("https://api.usa.gov/crime/fbi/sapi/api/agencies/") |>
  req_url_query(API_KEY = Sys.getenv("DATA_GOV_API_KEY")) |>
  req_headers(Accept = "application/json") |>
  req_retry(max_tries = 5) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  map_dfr(bind_rows) |>
  mutate(
    nibrs_start_date = mdy(nibrs_start_date),
    county_name = str_to_title(county_name),
    county_name = na_if(county_name, ""),
    agency_name = str_trim(agency_name)
  ) |>
  select(
    ori,
    agency_name,
    agency_type = agency_type_name,
    state_abbr,
    county_name,
    is_nibrs = nibrs,
    nibrs_start_dt = nibrs_start_date
  )

# subset to non-nibrs agencies
non_nibrs_agency <- all_ucr_agency |>
  filter(!is_nibrs)

# from offenses known, get 2020 population for each agency
# will join with nibrs participation from above using ori
ucr_pop <- okca |>
  filter(year == 2020) |>
  select(ori = ori9, population) |>
  as_tibble()

# join ucr agencies from fbi with ucr pop from offenses known
# note: not all rows match here
all_ucr_agency_pop <- all_ucr_agency |>
  left_join(ucr_pop, by = "ori")

# subset to agencies with missing population
mising_pop_agency <- all_ucr_agency_pop |>
  filter(is.na(population))

# this is going to be a long series of api calls, so wrap function
# to return results even if one iteration fails
safe_get_agency_pop <- safely(get_agency_pop)

# iterate over agencies oris with missing population and call api
# note: this takes a decent amount of time because we have to go one by one
# through 1000+ agencies
missing_pop_agency_api_res <- map(mising_pop_agency$ori, safe_get_agency_pop)

# clean up api results
# so agencies are missing from 2020 data so grab both 2020 and 2021
# if we have both, pick 2020 to align with offenses known population
missing_pop_agency_api_res <- missing_pop_agency_api_res |>
  map_dfr(pluck, "result") |>
  filter(data_year %in% 2020:2021) |>
  group_by(ori) |>
  slice_min(n = 1, order_by = data_year) |>
  ungroup() |>
  select(ori, population)

# update rows that were missing pop with newly downloaded api pop data
ucr_agency_pop_joined <- all_ucr_agency_pop |>
  rows_update(missing_pop_agency_api_res, by = "ori")

# write agency population and state nibrs population coverage to disk
write_rds(ucr_agency_pop_joined, file.path(sp_path, "ucr_agency_pop.rds"))
write_rds(state_nibrs_pop_cov, file.path(sp_path, "state_nibrs_pop_cov.rds"))
