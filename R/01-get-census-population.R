# get total and adult population for 2010-2021 from census via api/tidycensus

library(tidyverse)
library(tidycensus)
library(csgjcr)

# path to data folder on sharepoint
sp_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")

# make look up table with state fips, state name, and state abbreviation
fips_state_lookup <- fips_codes |>
  distinct(state_abb = state, state_fips = state_code) |>
  mutate(
    state = state.name[match(state_abb, state.abb)],
    state = case_when(
      state_abb == "DC" ~ "District of Columbia",
      state_abb == "AS" ~ "American Samoa",
      state_abb == "GU" ~ "Guam",
      state_abb == "MP" ~ "Northern Mariana Islands",
      state_abb == "PR" ~ "Puerto Rico",
      state_abb == "UM" ~ "United States Minor Outlying Islands",
      state_abb == "VI" ~ "United States Virgin Islands",
      TRUE ~ state
    )
  )

# define function to pull total popualtion for a given year and geography
get_acs_total_pop <- function(year, geography) {
  get_acs(
    geography = geography,
    variables = "B01003_001",  # https://censusreporter.org/tables/B01003/
    survey = "acs1",
    year = year
    ) |>
    transmute(year = year, state_fips = GEOID, pop_total = estimate)
}

# acs variables for population by age breakdown we need
# https://censusreporter.org/tables/B01001/
age_vars <- paste0("B01001_", str_pad(c(7:25, 31:49), width = 3, pad = "0"))

# define function to pull adult popualtion vars for a given year and geography
# then sum all age groups we have pulled to create total adult pop
get_acs_adult_pop <- function(year, geography) {
  ret <- get_acs(
    geography = geography,
    variables = age_vars,
    survey = "acs1",
    year = year
    ) |>
    group_by(year = year, state_fips = GEOID) |>
    summarize(pop_adult = sum(estimate)) |>
    ungroup()
}

# which acs years do we want data for
acs_years <- c(2011:2019, 2021)

# get total pop and acs pop for all specified years by state
acs_total_pop_by_state <- map_dfr(acs_years, ~ get_acs_total_pop(.x, "state"))
acs_adult_pop_by_state <- map_dfr(acs_years, ~ get_acs_adult_pop(.x, "state"))

# join adult and total pop data
acs_pop_by_state <- acs_total_pop_by_state |>
  left_join(acs_adult_pop_by_state, by = c("year", "state_fips"))

# get 2010 decenial census total and adult pop
# https://api.census.gov/data/2010/dec/sf1/variables/P001001.json
# https://api.census.gov/data/2010/dec/sf1/variables/P010001.json
state_pop_2010 <- get_decennial(
  geography = "state",
  variables = c("P010001", "P001001"),
  year = 2010,
  sumfile = "sf1",
  output = "wide"
  ) |>
  transmute(
    year = 2010,
    state_fips = GEOID,
    pop_total = P001001,
    pop_adult = P010001
    )

# get 2020 decenial census total and adult pop
# https://api.census.gov/data/2020/dec/pl/variables/P3_001N.json
# https://api.census.gov/data/2020/dec/pl/variables/P1_001N.json
state_pop_2020 <- get_decennial(
  geography = "state",
  variables = c("P3_001N", "P1_001N"),
  year = 2020,
  sumfile = "pl",
  output = "wide"
  ) |>
  transmute(
    year = 2020,
    state_fips = GEOID,
    pop_total = P1_001N,
    pop_adult = P3_001N
    )

# combine acs years, 2010 and 2020 population total and adult
# join with state fips state name lookup
state_pop_joined <- acs_pop_by_state |>
  bind_rows(state_pop_2010) |>
  bind_rows(state_pop_2020) |>
  arrange(state_fips, year) |>
  left_join(fips_state_lookup, by = "state_fips")

# write to disk
write_rds(state_pop_joined, file.path(sp_path, "/state_pop.rds"))
