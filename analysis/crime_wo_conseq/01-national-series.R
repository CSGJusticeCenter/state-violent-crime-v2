## Solved and unsolved violent crime in the US, 1965-2025
##
## Reported counts come from Crime Index state tables before 1979 and the FBI
## state estimates from 1979. Crime Index digitized its tables from the FBI's
## Crime in the United States.
##
## Solved counts:
## - 2015-2025 use the site's published method, us_solve_trend()
## - 1965-2014 rebuild that method from agency counts: one pooled clearance rate
##   from state totals, applied to US reported crime. The pool leaves out
##   state-years where agencies with at least min_screen_reported violent
##   offenses and no clearances hold at least max_no_clearance_share of reported
##   crime, and Illinois after illinois_last_year
##
## A year's clearances can include crimes from earlier years.
##
## Writes analysis/crime_wo_conseq/national-1965-2025.csv. The agency-based
## state rates and the Checks section only print comparisons and never reach
## the CSV. Run from the project root.

library(tidyverse)
library(csgjcr)
source("R/page-data.R")
source("analysis/crime_wo_conseq/helpers.R")

first_year <- 1965
fbi_count_year <- 1979
published_year <- 2015

### Illinois clearances are plausible through this year
illinois_last_year <- 1985

raw_path <- function(...) csg_sp_path("jr_data_library", "data", "raw", ...)
crimeindex_path <- function(file) raw_path("ahd", "crime_index", file)

srs <- read_srs_state()

### Agency clearances ---------------------------------------------------------

### Return A agency counts before 2000
kaplan <- read_rds(raw_path("fbi", "srs", "kaplan", "offenses_known_yearly_1960_2024.rds")) |>
  filter(between(year, first_year, 1999)) |>
  transmute(
    year,
    ori,
    state_abbr = as.character(as.vector(state_abb)),
    reported = as.vector(actual_index_violent),
    cleared = coalesce(as.vector(total_cleared_index_violent), 0)
  ) |>
  as_tibble()

### CDE agency counts from 2000, with NYPD's 2022 clearances repaired. The CSV
### uses agency counts only before published_year, so the repair reaches only
### the checks
nypd_clearance <- read_csv("data/nypd_clearance_2022.csv", show_col_types = FALSE)

cde <- read_rds(csg_sp_path("jr_data_library", "data", "analysis", "fbi", "srs", "fbi_srs_agency.rds")) |>
  filter(group %in% violent_offenses, indicator %in% c("Incidents reported", "Incidents cleared")) |>
  mutate(indicator = if_else(indicator == "Incidents reported", "reported", "cleared")) |>
  summarize(n = sum(n, na.rm = TRUE), .by = c(year, ori, state_abbr, group, indicator)) |>
  pivot_wider(names_from = indicator, values_from = n, values_fill = 0) |>
  left_join(nypd_clearance, by = join_by(year, ori, group), relationship = "one-to-one") |>
  mutate(cleared = if_else(is.na(clearance_rate), cleared, reported * clearance_rate)) |>
  summarize(across(c(reported, cleared), sum), .by = c(year, ori, state_abbr)) |>
  mutate(state_abbr = as.character(state_abbr))

agencies <- bind_rows(kaplan, cde) |>
  filter(reported > 0) |>
  mutate(non_reporting = reported >= min_screen_reported & cleared == 0)

### State reported counts -----------------------------------------------------

fbi_reported <- srs |>
  filter(crime_cat == "Violent", year >= fbi_count_year) |>
  summarize(incidents_reported = sum(incidents_reported), .by = c(year, state_abbr)) |>
  mutate(state_abbr = as.character(state_abbr))

crimeindex_crime <- read_csv(
  crimeindex_path("historical_crime_1930_2025_1790092670.csv"),
  col_types = cols(id = "c", .default = "d")
)

### Crime Index has no DC row, so DC is the US total less the 50 states
ci_reported <- crimeindex_crime |>
  filter(str_detect(id, "^[a-z]{2}$"), between(year, first_year, fbi_count_year - 1)) |>
  transmute(year, state_abbr = str_to_upper(id), incidents_reported = violent_total)

ci_reported <- ci_reported |>
  summarize(
    incidents_reported = sum(incidents_reported[state_abbr == "US"]) - sum(incidents_reported[state_abbr != "US"]),
    .by = year
  ) |>
  mutate(state_abbr = "DC") |>
  bind_rows(filter(ci_reported, state_abbr != "US"))

state_reported <- bind_rows(ci_reported, fbi_reported)

if (any(count(state_reported, year)$n != 51)) stop("Every year needs reported counts for 50 states and DC")
if (anyNA(state_reported$incidents_reported)) stop("Some state-years have no reported count")

### Site method from agency counts --------------------------------------------

site_replica_rates <- agencies |>
  filter(state_abbr %in% state_reported$state_abbr) |>
  summarize(
    share_no_clearances = sum(reported[non_reporting]) / sum(reported),
    across(c(reported, cleared), sum),
    .by = c(year, state_abbr)
  ) |>
  filter(
    share_no_clearances < max_no_clearance_share,
    !(state_abbr == "IL" & year > illinois_last_year)
  ) |>
  summarize(pct_site_replica = sum(cleared) / sum(reported), .by = year)

site_replica_us <- state_reported |>
  summarize(incidents_reported = sum(incidents_reported), .by = year) |>
  left_join(site_replica_rates, by = join_by(year), relationship = "one-to-one") |>
  mutate(incidents_solved = incidents_reported * pct_site_replica) |>
  select(-pct_site_replica)

### Agency-based state rates --------------------------------------------------

### for the checks: each state's rate from agencies that report clearances,
### applied to its own reported count. Illinois after illinois_last_year, and
### states with no clearances left, take the national rate

state_rates <- agencies |>
  filter(!non_reporting, state_abbr %in% state_reported$state_abbr) |>
  summarize(across(c(reported, cleared), sum), .by = c(year, state_abbr)) |>
  mutate(
    state_rate = if_else(cleared > 0, cleared / reported, NA),
    state_rate = if_else(state_abbr == "IL" & year > illinois_last_year, NA, state_rate)
  )

national_rates <- state_rates |>
  filter(!is.na(state_rate)) |>
  summarize(national_rate = sum(cleared) / sum(reported), .by = year)

agency_states <- state_reported |>
  left_join(select(state_rates, year, state_abbr, state_rate), by = join_by(year, state_abbr), relationship = "one-to-one") |>
  left_join(national_rates, by = join_by(year), relationship = "many-to-one") |>
  mutate(
    national_fallback = is.na(state_rate),
    incidents_solved = incidents_reported * coalesce(state_rate, national_rate)
  )

agency_us <- agency_states |>
  summarize(across(c(incidents_reported, incidents_solved), sum), .by = year)

### US population -------------------------------------------------------------

### the FBI state estimates carry population from 2000; homicide rows carry each
### state's population once
srs_population <- srs |>
  filter(group == "Homicide", !is.na(pop_total)) |>
  summarize(population = sum(pop_total), states = n(), .by = year)

if (any(srs_population$states != 51)) stop("Some FBI estimate years lack population for every state")

us_population <- crimeindex_crime |>
  filter(id == "us", year >= first_year, !year %in% srs_population$year) |>
  select(year, population) |>
  bind_rows(select(srs_population, year, population))

### Spliced series ------------------------------------------------------------

published <- us_solve_trend(srs, published_year) |>
  select(year, incidents_reported, incidents_solved)

trend <- bind_rows(
  `site method from agency counts` = filter(site_replica_us, year < published_year),
  `published site method` = published,
  .id = "solve_method"
) |>
  left_join(us_population, by = join_by(year), relationship = "one-to-one") |>
  mutate(
    count_source = if_else(year < fbi_count_year, "Crime Index", "FBI state estimates"),
    incidents_unsolved = incidents_reported - incidents_solved,
    pct_solved = incidents_solved / incidents_reported,
    across(
      c(incidents_reported, incidents_solved, incidents_unsolved),
      \(x) x / population * 1e5,
      .names = "{str_remove(.col, 'incidents_')}_per_100k"
    )
  ) |>
  select(year, count_source, solve_method, population, starts_with("incidents"), pct_solved, ends_with("per_100k")) |>
  arrange(year)

if (anyNA(trend$population)) stop("The spliced series has years without population")
if (anyNA(trend$incidents_solved)) stop("The spliced series has years without a solve rate")
if (!all(diff(trend$year) == 1)) stop("The spliced series skips or repeats a year")

write_csv(trend, national_path)

### Checks --------------------------------------------------------------------

### state-years that take the national rate, outside Illinois
agency_states |>
  filter(national_fallback, state_abbr != "IL", year < published_year) |>
  summarize(years = str_flatten(year, " "), .by = state_abbr) |>
  print()

### Crime Index's FBI-table clearance rates, which leave out agencies without clearances
crimeindex_offenses <- c(murder = "Homicide", rape = "Rape", robbery = "Robbery", assault = "Aggravated assault")
ci_counts <- crimeindex_crime |>
  filter(id == "us") |>
  select(year, str_c(names(crimeindex_offenses), "_total")) |>
  pivot_longer(-year, names_to = "offense", names_pattern = "(.*)_total", values_to = "reported")
ci_solve <- read_csv(crimeindex_path("historical_clearance_1960_2025_1790092670.csv"), col_types = cols(id = "c", .default = "d")) |>
  filter(id == "us") |>
  select(year, str_c(names(crimeindex_offenses), "_cleared_rate")) |>
  pivot_longer(-year, names_to = "offense", names_pattern = "(.*)_cleared_rate", values_to = "pct_cleared") |>
  inner_join(ci_counts, by = join_by(year, offense), relationship = "one-to-one") |>
  summarize(pct_crimeindex = sum(reported * pct_cleared / 100) / sum(reported), .by = year)

### agency rates, the site method on FBI-estimated clearances from their first
### year, the site method rebuilt from agency counts, and Crime Index. Near the
### published_year splice, the rebuilt and published rates should agree
site_method <- us_solve_trend(srs, 1985) |>
  transmute(year, pct_site_method = incidents_solved / incidents_reported)

agency_us |>
  transmute(year, pct_agency = incidents_solved / incidents_reported) |>
  left_join(site_method, by = join_by(year)) |>
  left_join(site_replica_rates, by = join_by(year)) |>
  left_join(ci_solve, by = join_by(year)) |>
  mutate(across(starts_with("pct"), \(x) round(100 * x, 1))) |>
  print(n = Inf)
