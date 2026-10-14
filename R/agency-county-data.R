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

# Flag agency-years whose counts suggest incomplete reporting.
# "no_violent_crime" marks agencies serving min_pop_no_violent_crime or more
# residents that report no violent crime. "no_clearances" marks agencies that
# report min_screen_reported or more violent offenses and clear none.
agency_data_flag <- function(reported, cleared, population) {
  case_when(
    reported == 0 & coalesce(population, 0) >= min_pop_no_violent_crime ~ "no_violent_crime",
    reported >= min_screen_reported & cleared == 0 ~ "no_clearances",
    .default = NA_character_
  )
}

# Build the three objects the state pages read.
#
# county_map: one row per primary county, with the five NYC boroughs merged
#   into nyc_fips. Statewide, tribal, county-less and flagged agencies stay
#   off the map.
# agency_table: one row per agency, with change in solve rate since base_year.
#   data_flag comes from agency_data_flag(). Flagged agencies have no crime
#   rate (no_violent_crime) or no solve rates (no_clearances) in either year.
# agency_offense_table: one row per agency, with counts and rates by offense,
#   blanked the same way for flagged agencies.
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
    mutate(
      data_flag = agency_data_flag(n_reported, n_solved, population),
      solved_rate = if_else(n_reported > 0 & is.na(data_flag), n_solved / n_reported, NA_real_)
    )

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
      n_reported_rate = if_else(population > 0 & !data_flag %in% "no_violent_crime",
                                n_reported / population * 1e5, NA_real_),
      solved_rate_change = (solved_rate - solved_rate_base) * 100,
      agency_year = agency_year,
      base_year = base_year
    ) |>
    select(
      state_name, state_abbr, ori, agency_name, county, county_fips, county_fips_all,
      county_method, population, n_reported, n_reported_rate, n_solved, solved_rate,
      solved_rate_change, data_flag, agency_year, base_year
    )

  county_map <- current |>
    filter(!is.na(county_fips), is.na(data_flag)) |>
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
      agency_table |> select(ori, county, population, data_flag),
      by = "ori"
    ) |>
    mutate(
      across(starts_with("solved_rate_"), \(x) if_else(is.na(data_flag), x, NA_real_)),
      across(starts_with("n_reported_rate_"), \(x) if_else(data_flag %in% "no_violent_crime", NA_real_, x)),
      agency_year = agency_year
    ) |>
    relocate(county, population, data_flag, .after = county_fips)

  list(
    county_map = county_map,
    agency_table = agency_table,
    agency_offense_table = agency_offense_table
  )
}

# Share of each state's population served by agencies that reported at least
# one month in coverage_year. pep has state_abbr, state_name and pop_total for
# that year. Agency populations can overlap, so a share can pass 1.
state_reporting_coverage <- function(raw, pep, coverage_year) {
  agencies <- raw |>
    filter(year == coverage_year, reporting_status != "none") |>
    distinct(state_abbr = as.character(state_abbr), ori, pop_covered)
  stopifnot("one population per agency" = !anyDuplicated(agencies$ori))

  agencies |>
    summarize(pop_reporting = sum(pop_covered, na.rm = TRUE), .by = state_abbr) |>
    right_join(mutate(pep, state_abbr = as.character(state_abbr)), by = "state_abbr") |>
    mutate(
      pop_reporting = coalesce(pop_reporting, 0),
      coverage = pop_reporting / pop_total,
      year = coverage_year
    ) |>
    select(year, state_abbr, state_name, pop_reporting, pop_total, coverage) |>
    arrange(state_abbr)
}

# Violent offenses reported and cleared by agency, year and offense, across every
# reporting status. The clearance screen and repairs judge the whole state.
agency_clearance_counts <- function(raw, years) {
  raw |>
    filter(
      year %in% years,
      group %in% violent_offenses,
      indicator %in% c("Incidents reported", "Incidents cleared")
    ) |>
    mutate(
      state_name = as.character(state_name),
      indicator = if_else(indicator == "Incidents reported", "reported", "cleared")
    ) |>
    summarize(n = sum(n, na.rm = TRUE), .by = c(year, state_name, ori, group, indicator)) |>
    pivot_wider(names_from = indicator, values_from = n, values_fill = 0)
}

# Share of each state's reported violent offenses that came from agencies
# reporting at least min_reported violent offenses and no clearances, by year.
clearance_reporting_gaps <- function(raw, years, min_reported = min_screen_reported) {
  agency_clearance_counts(raw, years) |>
    summarize(across(c(reported, cleared), sum), .by = c(year, state_name, ori)) |>
    mutate(no_clearances = reported >= min_reported & cleared == 0) |>
    summarize(
      share_no_clearances = sum(reported[no_clearances]) / sum(reported),
      .by = c(year, state_name)
    ) |>
    arrange(state_name, year)
}

# State clearance rates by offense with listed agencies' clearances replaced.
# repairs has year, ori, group and the agency's clearance_rate from another
# source. Every other agency keeps its reported clearances.
repair_agency_clearances <- function(raw, repairs) {
  incomplete <- repairs |>
    summarize(complete = n() == length(violent_offenses) && setequal(group, violent_offenses), .by = c(year, ori)) |>
    filter(!complete)
  if (nrow(incomplete) > 0) {
    stop("Repairs need one rate for every violent offense: ", paste(incomplete$ori, incomplete$year, collapse = ", "))
  }

  counts <- agency_clearance_counts(raw, unique(repairs$year))
  missing <- anti_join(distinct(repairs, year, ori), counts, by = c("year", "ori"))
  if (nrow(missing) > 0) {
    stop("Repaired agencies are missing from the agency file: ", paste(missing$ori, missing$year, collapse = ", "))
  }

  repaired_states <- counts |>
    semi_join(repairs, by = c("year", "ori")) |>
    distinct(year, state_name)

  counts |>
    semi_join(repaired_states, by = c("year", "state_name")) |>
    left_join(select(repairs, year, ori, group, clearance_rate), by = c("year", "ori", "group")) |>
    mutate(cleared = if_else(is.na(clearance_rate), cleared, reported * clearance_rate)) |>
    summarize(pct_cleared = sum(cleared) / sum(reported), .by = c(year, state_name, group)) |>
    arrange(state_name, year, group)
}
