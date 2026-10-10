## settings and data prep shared by the national and state pages

### pre-pandemic year that maps, tables and agency changes compare against
base_year <- 2019

### states without reliable clearance data in the base or latest year
states_to_exclude_from_solved_rate_viz <- c("Illinois")

### solve-rate trend charts skip 2021, when NIBRS transition cut reporting
solve_rate_skip_year <- 2021

### homicide charts pool SHR from this year on because coverage is low in 2021
shr_first_year <- 2022

### share of a trend's range added above and below it on the y-axis
axis_expansion_mult <- 0.5

### C-CPI-U index for inflation-adjusting expenditures
### from: https://www.census.gov/topics/income-poverty/income/guidance/current-vs-constant-dollars.html
inflation_index_path <- "data/annual-index-value_annual-percent-change_2025.xls"

### map palettes, diverging for changes and sequential for rates
change_colors <- c("#00475d", "#007392", "#9ed4ef", "#FFFFFF", "#EDB799", "#D25E2D", "#7B3014")
rate_colors <- c("#d4e9f8", "#9ed4ef", "#007392", "#00475d")

### this order sets the offense column order in the agency tables
violent_offenses <- c("Homicide", "Rape", "Robbery", "Aggravated assault")

### one row per state, year and offense with reported, unsolved and cleared counts
### raw is fbi_srs_estimated_crimes_state.rds from jr_data_library
prep_srs_state <- function(raw) {
  raw |>
    mutate(crime_cat = if_else(group %in% violent_offenses, "Violent", "Property")) |>
    select(-state_fips, -group_cat, -starts_with("rate")) |>
    filter(indicator != "Unsolved rate") |>
    pivot_wider(names_from = indicator, values_from = n) |>
    janitor::clean_names() |>
    select(
      year, state_name, state_abbr, group, crime_cat, pop_total,
      incidents_reported, incidents_unsolved, incidents_cleared
    ) |>
    mutate(incidents_reported_rate_total = incidents_reported / pop_total)
}

read_srs_state <- function() {
  read_rds(csg_sp_path("jr_data_library", "data", "analysis", "fbi", "srs", "fbi_srs_estimated_crimes_state.rds")) |>
    prep_srs_state()
}

add_solve_rates <- function(df) {
  mutate(
    df,
    incidents_solved = incidents_reported - incidents_unsolved,
    pct_solved = incidents_solved / incidents_reported
  )
}

### violent crime rows from first_year on for one state, or every state
### labeled as the United States when state is NULL
violent_rows <- function(srs, first_year, state = NULL) {
  srs <- filter(srs, crime_cat == "Violent", year >= first_year)
  if (is.null(state)) {
    mutate(srs, state_name = "United States", state_abbr = "U.S.")
  } else {
    filter(srs, state_name == state)
  }
}

### all violent crime by year for one state, or the US when state is NULL
### homicide rows carry the population once per state
violent_totals <- function(srs, first_year, state = NULL) {
  violent_rows(srs, first_year, state) |>
    group_by(year, state_name, state_abbr) |>
    summarize(
      pop_total = sum(pop_total[group == "Homicide"], na.rm = TRUE),
      across(c(incidents_reported, incidents_unsolved), \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(incidents_reported_rate_total = incidents_reported / pop_total) |>
    add_solve_rates()
}

### violent crime by year and offense for one state, or the US when state is NULL
violent_by_offense <- function(srs, first_year, state = NULL) {
  rows <- violent_rows(srs, first_year, state)
  if (!is.null(state)) return(add_solve_rates(rows))

  rows |>
    group_by(year, state_name, state_abbr, group) |>
    summarize(
      across(c(pop_total, incidents_reported, incidents_unsolved), \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(incidents_reported_rate_total = incidents_reported / pop_total) |>
    add_solve_rates()
}

### pooled US solve rate by offense in one year, named by snake_case offense
### excluded states are left out of the pool
us_solve_benchmark <- function(srs, year, exclude = states_to_exclude_from_solved_rate_viz) {
  srs |>
    filter(!state_name %in% exclude) |>
    violent_by_offense(year) |>
    filter(year == !!year) |>
    pull(pct_solved, name = group) |>
    set_names(janitor::make_clean_names)
}

### first 4 characters of year drop footnote markers (e.g., "20252" is 2025)
read_inflation_index <- function(path = inflation_index_path) {
  readxl::read_xls(path, skip = 2) |>
    select(year = 1, inflation_index = 2) |>
    mutate(year = as.numeric(str_sub(year, 1, 4))) |>
    filter(!is.na(year), !is.na(inflation_index))
}

### convert n to dollars of the latest year in df
adjust_for_inflation <- function(df, inflation_index) {
  df |>
    left_join(inflation_index, by = "year") |>
    mutate(n_inflation_adjusted_latest_year_dollars = n * inflation_index[year == max(year)] / inflation_index)
}

### state and local police protection expenditures for the last 11 survey years
read_police_spending <- function(geo, inflation_index) {
  read_rds(csg_sp_path("jr_data_library", "data", "analysis", "census", "asslgf", "census_asslgf_state.rds")) |>
    filter(
      indicator == "Expenditures",
      group == "Police Protection",
      geo_abbr == geo,
      year >= max(year) - 10
    ) |>
    adjust_for_inflation(inflation_index)
}

### sworn officer counts and rates for the last 11 years
read_lee_officers <- function(geo) {
  read_rds(csg_sp_path("jr_data_library", "data", "analysis", "fbi", "lee", "fbi_lee_state.rds")) |>
    filter(geo_abbr %in% geo, group == "Officers", year >= max(year) - 10)
}

### officer rates use the population covered by reporting agencies
### officers per violent crime divides that rate by violent_rate, crimes per resident
add_officer_rates <- function(lee) {
  mutate(
    lee,
    officer_pop_rate_per_10k = rate_covered * 10000,
    officer_per_violent_offense_1k = rate_covered / violent_rate * 1000
  )
}
