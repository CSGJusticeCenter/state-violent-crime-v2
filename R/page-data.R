## settings and data prep shared by the national and state pages

### pre-pandemic year that maps, tables and agency changes compare against
base_year <- 2019

### first year of reliable clearance data for states whose clearances start after
### the earliest year shown. Earlier solve rates are hidden and left out of US
### solve rates
solve_rate_first_years <- c(Illinois = 2022)

### the NIBRS transition cut reporting in 2021, so solve rates for that year show
### only where reporting agencies covered at least min_solve_coverage of the population
### coverage comes from R/download-ucr-coverage.R
low_coverage_solve_year <- 2021
min_solve_coverage <- 0.7
solve_coverage_path <- "data/ucr_coverage_2021.csv"

### homicide charts pool SHR from this year on because coverage is low in 2021
shr_first_year <- 2022

### the clearance screen hides solve rates for state-years where agencies that
### reported at least min_screen_reported violent offenses but no clearances hold
### at least max_no_clearance_share of the state's reported violent offenses
### R/01-prep-agency-county-data.R writes the screen and the repaired rates that
### replace failed clearances
max_no_clearance_share <- 0.05
min_screen_reported <- 10
clearance_screen_path <- "data/clearance_screen.rds"
clearance_repairs_path <- "data/clearance_repairs.rds"

### repaired offenses the pages show. The others count toward the violent crime
### total but have no solve rate of their own
repaired_offenses_shown <- c("Robbery", "Aggravated assault")

### label for the all-offense total in solve-rate exclusions and exports
total_label <- "Violent crime"

### the methodology names states where reporting agencies cover less than this
### share of the population
low_coverage_threshold <- 0.9

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

read_srs_state <- function(screen = read_rds(clearance_screen_path),
                           repairs = read_rds(clearance_repairs_path)) {
  read_rds(csg_sp_path("jr_data_library", "data", "analysis", "fbi", "srs", "fbi_srs_estimated_crimes_state.rds")) |>
    prep_srs_state() |>
    repair_clearances(repairs) |>
    flag_screened_solve_rates(screen, repairs)
}

### replace cleared and unsolved counts where repairs give a state clearance rate
repair_clearances <- function(srs, repairs) {
  key <- paste(srs$year, srs$state_name, srs$group)
  rate <- repairs$pct_cleared[match(key, paste(repairs$year, repairs$state_name, repairs$group))]
  mutate(
    srs,
    incidents_cleared = if_else(is.na(rate), incidents_cleared, incidents_reported * rate),
    incidents_unsolved = if_else(is.na(rate), incidents_unsolved, incidents_reported - incidents_cleared)
  )
}

### TRUE for rows before their state's entry in first_years
before_first_solve_year <- function(df, first_years = solve_rate_first_years) {
  first <- unname(first_years[as.character(df$state_name)])
  coalesce(df$year < first, FALSE)
}

### hide_total_solve marks state-years whose violent crime solve rate the pages hide
### hide_offense_solve marks offense rows whose own solve rate the pages hide
### screened state-years without repairs, and years before a state's first solve
### year, hide both. Repaired offenses outside repaired_offenses_shown hide only
### their own rate
flag_screened_solve_rates <- function(srs, screen, repairs,
                                      max_share = max_no_clearance_share,
                                      shown = repaired_offenses_shown,
                                      first_years = solve_rate_first_years) {
  repaired_years <- distinct(repairs, year, state_name)
  incomplete <- count(repairs, year, state_name) |> filter(n != length(violent_offenses))
  if (nrow(incomplete) > 0) stop("Clearance repairs need every violent offense for a state-year")

  ### a year without screen rows would pass unscreened
  shown_years <- intersect(unique(srs$year), (max(srs$year) - 10):max(srs$year))
  unscreened <- setdiff(shown_years, screen$year)
  if (length(unscreened) > 0) {
    stop(
      "The clearance screen has no rows for ", paste(sort(unscreened), collapse = ", "),
      ". Rerun R/01-prep-agency-county-data.R with a later agency_year"
    )
  }

  failed <- screen |>
    filter(share_no_clearances >= max_share) |>
    anti_join(repaired_years, by = c("year", "state_name"))
  failed_key <- paste(failed$year, failed$state_name)
  hidden_repairs <- filter(repairs, !group %in% shown)
  hidden_key <- paste(hidden_repairs$year, hidden_repairs$state_name, hidden_repairs$group)

  early <- before_first_solve_year(srs, first_years)
  flagged <- mutate(
    srs,
    hide_total_solve = early | paste(year, state_name) %in% failed_key,
    hide_offense_solve = hide_total_solve | paste(year, state_name, group) %in% hidden_key
  )

  ### maps and narrative compare these years, so a hidden rate in one needs a decision
  ### years before a state's first solve year are already decided
  compared <- c(base_year, max(srs$year) - 10, max(srs$year))
  conflicts <- flagged[!early, ] |>
    filter(crime_cat == "Violent", year %in% compared, hide_offense_solve) |>
    distinct(year, state_name)
  if (nrow(conflicts) > 0) {
    stop(
      "The clearance screen hides solve rates in a compared year: ",
      paste(conflicts$state_name, conflicts$year, collapse = ", ")
    )
  }
  flagged
}

### TRUE for rows whose solve rates the clearance screen hides
### rows without a group, or with total_label, are violent crime totals
screened_out <- function(df) {
  column <- function(name) if (name %in% names(df)) df[[name]] else rep(NA, nrow(df))
  total <- if ("group" %in% names(df)) df$group %in% total_label else rep(TRUE, nrow(df))
  coalesce(if_else(total, column("hide_total_solve"), column("hide_offense_solve")), FALSE)
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
### a state keeps its hide_total_solve flag, and the US never has one
violent_totals <- function(srs, first_year, state = NULL) {
  flag <- if (is.null(state)) character() else "hide_total_solve"
  violent_rows(srs, first_year, state) |>
    group_by(year, state_name, state_abbr) |>
    summarize(
      pop_total = sum(pop_total[group == "Homicide"], na.rm = TRUE),
      across(c(incidents_reported, incidents_unsolved), \(x) sum(x, na.rm = TRUE)),
      across(any_of(flag), any),
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

### states pooled into US solve rates, leaving out rows with hidden solve rates
### US crime counts and rates pool every state
solve_rate_pool <- function(srs, by_offense = FALSE) {
  hidden <- if (by_offense) "hide_offense_solve" else "hide_total_solve"
  if (!hidden %in% names(srs)) return(srs)
  srs[!coalesce(srs[[hidden]], FALSE), ]
}

### US violent crime by year, or by year and offense when by_offense is TRUE
### the solve rate pools the states solve_rate_pool keeps, and solved and
### unsolved counts apply that rate to the reported count for every state
us_solve_trend <- function(srs, first_year, by_offense = FALSE) {
  summarize_us <- if (by_offense) violent_by_offense else violent_totals
  pooled_rates <- solve_rate_pool(srs, by_offense) |>
    summarize_us(first_year) |>
    select(any_of(c("year", "group")), pct_solved)

  us <- summarize_us(srs, first_year) |>
    select(-incidents_unsolved, -incidents_solved, -pct_solved) |>
    left_join(pooled_rates, by = setdiff(names(pooled_rates), "pct_solved")) |>
    mutate(
      incidents_solved = incidents_reported * pct_solved,
      incidents_unsolved = incidents_reported - incidents_solved
    )

  ### a missing pooled rate would print "NA%" in tooltips
  if (anyNA(us$pct_solved)) stop("US solve trend has years or offenses without a pooled solve rate")
  us
}

### pooled US solve rate by offense in one year, named by snake_case offense
us_solve_benchmark <- function(srs, year) {
  solve_rate_pool(srs, by_offense = TRUE) |>
    violent_by_offense(year) |>
    filter(year == !!year) |>
    pull(pct_solved, name = group) |>
    set_names(janitor::make_clean_names)
}

### population coverage by state, DC and the United States in low_coverage_solve_year
read_solve_coverage <- function(path = solve_coverage_path) {
  read_csv(path, show_col_types = FALSE) |>
    select(state_name, coverage)
}

### TRUE for rows in low_coverage_solve_year, with that year's coverage
solve_coverage_rows <- function(df, coverage_tbl, coverage_year = low_coverage_solve_year) {
  in_year <- df$year == coverage_year
  row_coverage <- coverage_tbl$coverage[match(as.character(df$state_name), coverage_tbl$state_name)]
  if (anyNA(row_coverage[in_year])) stop("Missing ", coverage_year, " coverage for some solve-rate rows")
  list(in_year = in_year, coverage = row_coverage)
}

### TRUE for rows whose solve rates the site hides in low_coverage_solve_year
low_solve_coverage <- function(df, coverage_tbl, min_coverage = min_solve_coverage) {
  rows <- solve_coverage_rows(df, coverage_tbl)
  rows$in_year & rows$coverage < min_coverage
}

### TRUE for rows whose solve rates the site hides for low 2021 coverage, the
### clearance screen or a later first solve year
solve_rate_hidden <- function(df, coverage_tbl) {
  low_solve_coverage(df, coverage_tbl) | screened_out(df)
}

### solve-rate trend rows, dropping rows solve_rate_hidden marks
### coverage_note is a tooltip line for the rows that stay in low_coverage_solve_year
solve_rate_years <- function(df, coverage_tbl) {
  rows <- solve_coverage_rows(df, coverage_tbl)
  df |>
    mutate(coverage_note = if_else(
      rows$in_year,
      sprintf("<br>Agencies covering %.0f%% of the population reported in %d", rows$coverage * 100, low_coverage_solve_year),
      ""
    )) |>
    filter(!solve_rate_hidden(df, coverage_tbl))
}

### methodology note on Illinois clearances and its first solve year
illinois_solve_note <- function(coverage_tbl, first_year = solve_rate_first_years[["Illinois"]]) {
  coverage <- coverage_tbl$coverage[coverage_tbl$state_name == "Illinois"]
  paste0(
    "Illinois agencies reported crimes but almost no clearances to the FBI through 2020. ",
    "They began reporting clearances in ", low_coverage_solve_year, ", when agencies covering ",
    sprintf("%.0f", coverage * 100), " percent of the state's population reported. ",
    "Illinois solve rates therefore start in ", first_year, ", and U.S. solve rates include Illinois from ",
    first_year, " on. Before ", first_year, ", U.S. solve rates leave out Illinois."
  )
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
