## settings and data prep shared by the national and state pages

### path inside the SharePoint data library, whose folder name is capitalized
### differently on different synced machines
jr_data_path <- function(...) {
  root <- csgjcr::csg_sp_path()
  lib <- grep("^jr_data_library$", list.files(root), ignore.case = TRUE, value = TRUE)
  if (length(lib) == 0) {
    stop("No jr_data_library folder found in ", root, call. = FALSE)
  }
  file.path(root, lib[1], ...)
}

### pre-pandemic year that maps, tables and agency changes compare against
base_year <- 2019

### states without reliable clearance data in the base or latest year
states_to_exclude_from_solved_rate_viz <- c("Illinois")

### state page setup shared by the full and backlog-only pages
### abbr is the page's state param, e.g., "DC"; state is the full name
state_name_from_abbr <- function(abbr) csgjcr::csg_state_convert(abbr, from = "abbr", "name")
state_place_text <- function(abbr) {
  if (abbr == "DC") "the District of Columbia" else state_name_from_abbr(abbr)
}
shows_solve_rates <- function(state) !state %in% states_to_exclude_from_solved_rate_viz

### shown in place of solve-rate sections on pages without solve-rate data
no_solve_data_sentence <- function(place) {
  paste(place, "did not report enough data on solved crime to the FBI to make reliable estimates.")
}

### a mid-document YAML block setting show-solved, which shows or hides the
### solve-rate sections; when-meta needs a YAML boolean, and an inline-R value
### in the title block arrives as a string
show_solved_meta <- function(show_solved) {
  cat("---\nshow-solved: ", tolower(show_solved), "\n---\n", sep = "")
}

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

### agencies serving at least min_pop_no_violent_crime residents that report no
### violent crime, or reporting at least min_screen_reported violent offenses and
### no clearances, are flagged as incomplete in the agency tables and left off the map
min_pop_no_violent_crime <- 10000

### solve rates from fewer than min_rate_reported incidents show in gray in the
### agency tables and drop from the homicide panels
min_rate_reported <- 10

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

### every chart and map color, from the Crime Without Consequence microsite's
### 50-state crime data page, plus orange for solve rates below the comparison
### rate; muted text keeps 4.5:1 contrast on white
site_colors <- c(
  navy = "#001E35", dark_blue = "#004270", blue = "#005DA6", slate = "#7397B0",
  teal = "#2DEBC3", dark_teal = "#17B894", light_teal = "#C8F5EA", pale_blue = "#D7E3EE",
  white = "#FFFFFF", text = "#000000", dark_gray = "#33404D", muted = "#5A6470",
  baseline = "#9FB0C0", border = "#C9D3DC", grid = "#DDE3E9",
  orange = "#9A4A0A", light_orange = "#FBE8DA"
)

### map palettes, diverging for changes (navy to teal) and sequential for rates
change_colors <- unname(site_colors[c("navy", "blue", "pale_blue", "white", "light_teal", "teal", "dark_teal")])
rate_colors <- unname(site_colors[c("pale_blue", "slate", "blue", "navy")])

### county map solve-rate classes, in whole percents
### the breaks split 2025 counties with 25 or more incidents into groups of similar size
county_map_breaks <- c(35, 45, 55, 65)
county_map_colors <- unname(site_colors[c("pale_blue", "baseline", "slate", "blue", "navy")])

### counties without a rate are white, so they stand apart from every class
county_map_null_color <- site_colors[["white"]]
county_map_border <- list(color = site_colors[["text"]], width = 0.45)

### backlog charts sum unsolved crime from this year through the latest year
backlog_first_year <- 2020

### offense colors for every chart, in the backlog charts' stack order, bottom to top
offense_colors <- c(
  "Aggravated assault" = site_colors[["blue"]],
  "Robbery"            = site_colors[["slate"]],
  "Rape"               = site_colors[["teal"]],
  "Homicide"           = site_colors[["navy"]]
)
offense_pal <- tibble(color = unname(offense_colors), crime = names(offense_colors))
offense_color <- function(offense) unname(offense_colors[offense])

backlog_colors <- c(reported = site_colors[["navy"]], solved = site_colors[["blue"]],
                    unsolved = site_colors[["pale_blue"]], unsplit = site_colors[["baseline"]])

### violent offenses in the order of the agency data columns and data exports
### page tables order offenses by table_offenses in R/utils.R
violent_offenses <- c("Homicide", "Rape", "Robbery", "Aggravated assault")

### one row per state, year and offense with reported, unsolved and cleared counts
### raw is fbi_srs_estimated_crimes_state.rds from jr_data_library
### pop_total in raw is Census PEP state population, not an FBI figure
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
  read_rds(jr_data_path("data", "analysis", "fbi", "srs", "fbi_srs_estimated_crimes_state.rds")) |>
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

### hide_total_solve marks state-years whose violent crime solve rate the pages hide
### hide_offense_solve marks offense rows whose own solve rate the pages hide
### screened state-years without repairs hide both. Repaired offenses outside
### repaired_offenses_shown hide only their own rate
flag_screened_solve_rates <- function(srs, screen, repairs,
                                      max_share = max_no_clearance_share,
                                      shown = repaired_offenses_shown,
                                      exclude = states_to_exclude_from_solved_rate_viz) {
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

  flagged <- mutate(
    srs,
    hide_total_solve = paste(year, state_name) %in% failed_key,
    hide_offense_solve = hide_total_solve | paste(year, state_name, group) %in% hidden_key
  )

  ### maps and narrative compare these years, so a hidden rate in one needs a decision
  compared <- c(base_year, max(srs$year) - 10, max(srs$year))
  conflicts <- flagged |>
    filter(crime_cat == "Violent", year %in% compared, hide_offense_solve, !state_name %in% exclude) |>
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

### states pooled into US solve rates, leaving out the excluded states and rows
### the clearance screen hides. US crime counts and rates pool every state
solve_rate_pool <- function(srs, exclude = states_to_exclude_from_solved_rate_viz, by_offense = FALSE) {
  hidden <- if (by_offense) "hide_offense_solve" else "hide_total_solve"
  keep <- !srs$state_name %in% exclude
  if (hidden %in% names(srs)) keep <- keep & !coalesce(srs[[hidden]], FALSE)
  srs[keep, ]
}

### US violent crime by year, or by year and offense when by_offense is TRUE
### the solve rate pools every state except the excluded ones, and solved and
### unsolved counts apply that rate to the reported count for every state
us_solve_trend <- function(srs, first_year, by_offense = FALSE,
                           exclude = states_to_exclude_from_solved_rate_viz) {
  summarize_us <- if (by_offense) violent_by_offense else violent_totals
  pooled_rates <- solve_rate_pool(srs, exclude, by_offense) |>
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
us_solve_benchmark <- function(srs, year, exclude = states_to_exclude_from_solved_rate_viz) {
  solve_rate_pool(srs, exclude, by_offense = TRUE) |>
    violent_by_offense(year) |>
    filter(year == !!year) |>
    pull(pct_solved, name = group) |>
    set_names(janitor::make_clean_names)
}

### crime rate per 100k residents by offense in one year, named by snake_case offense
### rows is violent_by_offense() output for one state or the US
offense_rate_benchmark <- function(rows, year) {
  rows <- filter(rows, year == !!year)
  if (nrow(rows) == 0) stop("No ", year, " crime rates by offense", call. = FALSE)
  rows |>
    mutate(rate_100k = incidents_reported_rate_total * 1e5) |>
    pull(rate_100k, name = group) |>
    set_names(janitor::make_clean_names)
}

### population, violent crime incidents, rate per 100k and solve rate in year
### `to`, with changes since `from`
### rates is violent_totals() output; solves defaults to rates, and the US page
### passes us_solve_trend() output; hidden solve rates are NA
### rate_change is in percent and solve_change in percentage points
total_rate_benchmark <- function(rates, from, to, solves = rates) {
  one_row <- function(df, y) {
    row <- df[df$year == y, ]
    if (nrow(row) != 1) stop("Expected one ", y, " row, found ", nrow(row), call. = FALSE)
    row
  }
  solve_rate <- function(y) {
    row <- one_row(solves, y)
    if (isTRUE(row[["hide_total_solve"]])) NA_real_ else row$pct_solved
  }
  latest <- one_row(rates, to)
  list(
    population = latest$pop_total,
    incidents = latest$incidents_reported,
    rate_100k = latest$incidents_reported_rate_total * 1e5,
    rate_change = pct_change_between(rates$incidents_reported_rate_total, rates$year, from, to) * 100,
    solve_rate = solve_rate(to),
    solve_change = (solve_rate(to) - solve_rate(from)) * 100
  )
}

### one state's solve rate by offense in one year, named by snake_case offense
### rates come from the state estimates the charts use; screened offenses are NA
state_solve_benchmark <- function(srs, state, year) {
  rows <- filter(srs, crime_cat == "Violent", state_name == state, year == !!year)
  if (nrow(rows) == 0) stop("No ", year, " solve rates for ", state, call. = FALSE)
  rows |>
    add_solve_rates() |>
    mutate(pct_solved = if_else(screened_out(rows), NA_real_, pct_solved)) |>
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
### clearance screen or an excluded state
solve_rate_hidden <- function(df, coverage_tbl, exclude = states_to_exclude_from_solved_rate_viz) {
  low_solve_coverage(df, coverage_tbl) | screened_out(df) | df$state_name %in% exclude
}

### solve-rate trend rows, dropping rows solve_rate_hidden marks
### excluded states skip these charts, so this keeps their rows
### coverage_note is a tooltip line for the rows that stay in low_coverage_solve_year
solve_rate_years <- function(df, coverage_tbl) {
  rows <- solve_coverage_rows(df, coverage_tbl)
  df |>
    mutate(coverage_note = if_else(
      rows$in_year,
      sprintf("<br>Agencies covering %.0f%% of the population reported in %d", rows$coverage * 100, low_coverage_solve_year),
      ""
    )) |>
    filter(!solve_rate_hidden(df, coverage_tbl, exclude = character()))
}

### rows from `from` on with backlog, the unsolved crime summed since then, by
### group when df has one. df is violent_totals(), violent_by_offense() or
### us_solve_trend() output
### years whose solve rate solve_rate_hidden() marks have no solved or unsolved
### count and add nothing to the backlog, so a place missing a year shows a minimum
### offenses hidden in a year whose total solve rate shows, such as New York's
### 2022 homicide and rape, also move into an unsplit row (see unsplit_backlog_rows())
add_backlog <- function(df, coverage_tbl, from = backlog_first_year) {
  df <- filter(df, year >= from)
  hidden <- solve_rate_hidden(df, coverage_tbl)
  if ("group" %in% names(df)) {
    ### total_label rows read the state-year's total solve flags
    total_hidden <- solve_rate_hidden(mutate(df, group = total_label), coverage_tbl)
    unsplit <- unsplit_backlog_rows(df[hidden & !total_hidden, ], unique(df$year))
    df <- bind_rows(mutate(df, unsplit = FALSE), unsplit)
    hidden <- c(hidden, rep(FALSE, NROW(unsplit)))
  }
  df |>
    mutate(
      incidents_solved = if_else(hidden, NA_real_, incidents_solved),
      incidents_unsolved = if_else(hidden, NA_real_, incidents_unsolved)
    ) |>
    arrange(year) |>
    mutate(backlog = cumsum(coalesce(incidents_unsolved, 0)), .by = any_of("group"))
}

### one row per year holding the solved and unsolved counts of offenses whose own
### solve rates are hidden, grouped as, e.g., "Homicide and rape, not broken out",
### or NULL when there are none; reported counts stay in the offense rows
unsplit_backlog_rows <- function(rows, years) {
  if (nrow(rows) == 0) return(NULL)
  label <- paste0(str_to_sentence(knitr::combine_words(tolower(sort(unique(rows$group))))),
                  ", not broken out")
  counts <- summarize(rows, across(c(incidents_solved, incidents_unsolved), sum), .by = year)
  tibble(year = years) |>
    left_join(counts, by = "year") |>
    mutate(group = label, unsplit = TRUE)
}

### a state's backlog totals by year, summed from add_backlog() offense rows so
### the total always equals its offenses; a year's solved and unsolved counts
### show only when every offense has a count or an unsplit row covers it
backlog_totals <- function(offenses) {
  unsplit <- if ("unsplit" %in% names(offenses)) offenses$unsplit else FALSE
  offenses |>
    mutate(unsplit = unsplit) |>
    summarize(
      incidents_reported = sum(incidents_reported, na.rm = TRUE),
      across(c(incidents_solved, incidents_unsolved), \(x) {
        if (any(is.na(x[!unsplit])) && !any(!is.na(x[unsplit]))) NA_real_ else sum(x, na.rm = TRUE)
      }),
      backlog = sum(backlog),
      .by = year
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
  read_rds(jr_data_path("data", "analysis", "census", "asslgf", "census_asslgf_state.rds")) |>
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
  read_rds(jr_data_path("data", "analysis", "fbi", "lee", "fbi_lee_state.rds")) |>
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
