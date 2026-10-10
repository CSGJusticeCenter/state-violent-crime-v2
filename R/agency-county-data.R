library(dplyr)
library(tidyr)
library(stringr)

### violent_offenses comes from R/page-data.R

# Collapse "full" agency rows to one row per agency, year and offense.
# Population is repeated across offenses, so it is taken once per agency-year.
agency_offense_counts <- function(raw, years) {
  raw |>
    filter(
      year %in% years,
      group %in% violent_offenses,
      reporting_status == "full",
      indicator %in% c("Incidents reported", "Incidents cleared")
    ) |>
    mutate(indicator = if_else(indicator == "Incidents reported", "reported", "cleared")) |>
    select(
      year, state_name, state_abbr, ori, agency_name, county_fips, county_fips_all,
      county_method, group, indicator, n, pop_covered
    ) |>
    pivot_wider(names_from = indicator, values_from = n)
}

# Stop when pivoting left duplicate agency-offense rows or a missing indicator.
check_agency_offense_counts <- function(counts) {
  stopifnot(
    "duplicate agency-year-offense rows" = !anyDuplicated(counts[c("year", "ori", "group")]),
    "missing reported or cleared counts" = !anyNA(counts[c("reported", "cleared")])
  )
  counts
}

# Suffixes dropped from county names in the agency tables.
county_suffix <- regex(
  " (County|Parish|Planning Region|City and Borough|Borough|Census Area|Municipality)$",
  ignore_case = TRUE
)

# Map each FIPS in county_fips_all to a bare county name.
# Names come from the agency file's own primary counties first, so Connecticut's
# planning regions resolve, then from tidycensus for counties no agency is based in.
county_name_lookup <- function(raw) {
  from_file <- raw |>
    filter(!is.na(county_fips)) |>
    distinct(county_fips, county_name) |>
    mutate(county_name = str_remove(county_name, county_suffix))

  from_census <- tidycensus::fips_codes |>
    transmute(
      county_fips = paste0(state_code, county_code),
      county_name = str_remove(county, county_suffix)
    ) |>
    filter(!county_fips %in% from_file$county_fips)

  bind_rows(from_file, from_census)
}

# Turn a ", "-separated FIPS string into a ", "-separated name string.
county_names_from_fips <- function(fips_all, lookup) {
  vapply(fips_all, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    }
    codes <- str_split_1(x, ", ")
    matched <- lookup$county_name[match(codes, lookup$county_fips)]
    paste(if_else(is.na(matched), codes, matched), collapse = ", ")
  }, character(1), USE.NAMES = FALSE)
}

# Build the three objects the state pages read.
#
# county_map: one row per primary county, with the five NYC boroughs merged
#   into nyc_fips. Statewide, tribal and county-less agencies have NA FIPS
#   and stay off the map.
# agency_table: one row per agency, with change in solve rate since base_year.
# agency_offense_table: one row per agency, with counts and rates by offense.
build_agency_county_data <- function(raw, agency_year, base_year) {
  counts <- agency_offense_counts(raw, c(base_year, agency_year)) |>
    check_agency_offense_counts()
  lookup <- county_name_lookup(raw)

  agency_total <- counts |>
    group_by(year, state_name, state_abbr, ori, agency_name, county_fips,
             county_fips_all, county_method) |>
    summarize(
      population = first(na.omit(pop_covered)),
      n_reported = sum(reported),
      n_solved = sum(cleared),
      .groups = "drop"
    ) |>
    mutate(solved_rate = if_else(n_reported > 0, n_solved / n_reported, NA_real_))

  base_rates <- agency_total |>
    filter(year == base_year) |>
    select(ori, solved_rate_base = solved_rate)

  current <- agency_total |> filter(year == agency_year)

  agency_table <- current |>
    left_join(base_rates, by = "ori") |>
    mutate(
      county = case_when(
        county_method == "statewide" ~ "Statewide",
        county_method == "tribal" ~ "Tribal",
        is.na(county_fips_all) ~ "None",
        TRUE ~ county_names_from_fips(county_fips_all, lookup)
      ),
      n_reported_rate = if_else(population > 0, n_reported / population * 1e5, NA_real_),
      solved_rate_change = (solved_rate - solved_rate_base) * 100,
      agency_year = agency_year,
      base_year = base_year
    ) |>
    select(
      state_name, state_abbr, ori, agency_name, county, county_fips, county_fips_all,
      county_method, population, n_reported, n_reported_rate, n_solved, solved_rate,
      solved_rate_change, agency_year, base_year
    )

  county_map <- current |>
    filter(!is.na(county_fips)) |>
    mutate(county_fips = if_else(county_fips %in% nyc_borough_fips, nyc_fips, county_fips)) |>
    group_by(state_name, state_abbr, county_fips) |>
    summarize(
      n_reported = sum(n_reported),
      n_solved = sum(n_solved),
      n_agencies = n_distinct(ori),
      pop_covered = sum(population, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      solved_rate = if_else(n_reported > 0, n_solved / n_reported, NA_real_),
      agency_year = agency_year
    )

  agency_offense_table <- counts |>
    filter(year == agency_year) |>
    mutate(
      offense = factor(str_replace_all(str_to_lower(group), " ", "_"),
                       levels = str_replace_all(str_to_lower(violent_offenses), " ", "_")),
      solved_rate = if_else(reported > 0, cleared / reported, NA_real_),
      reported_rate = if_else(pop_covered > 0, reported / pop_covered * 1e5, NA_real_)
    ) |>
    select(state_name, state_abbr, ori, agency_name, county_fips, offense,
           n_reported = reported, n_solved = cleared, n_reported_rate = reported_rate,
           solved_rate) |>
    pivot_wider(
      names_from = offense,
      values_from = c(n_reported, n_solved, n_reported_rate, solved_rate),
      names_glue = "{.value}_{offense}",
      names_expand = TRUE
    ) |>
    left_join(
      agency_table |> select(ori, county, population),
      by = "ori"
    ) |>
    mutate(agency_year = agency_year) |>
    relocate(county, population, .after = county_fips)

  list(
    county_map = county_map,
    agency_table = agency_table,
    agency_offense_table = agency_offense_table
  )
}
