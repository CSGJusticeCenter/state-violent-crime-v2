## US solve rates by violent offense, 1966-2025
##
## Same splice as solved-unsolved-since-1960.R, run per offense:
## - 2015-2025 use the site's published method, us_solve_trend(by_offense = TRUE)
## - 1966-2014 pool agency cleared and reported counts by offense across the
##   state-years that pass the site's clearance screen on violent totals, leaving
##   out Illinois after illinois_last_year

library(tidyverse)
library(csgjcr)
source("R/page-data.R")

first_year <- 1966
published_year <- 2015
illinois_last_year <- 1985

kaplan_offenses <- c(
  Homicide = "murder",
  Rape = "rape_total",
  Robbery = "robbery_total",
  `Aggravated assault` = "assault_aggravated"
)

### Agency counts by offense --------------------------------------------------

kaplan_raw <- read_rds(csg_sp_path("jr_data_library", "data", "raw", "fbi", "srs", "kaplan", "offenses_known_yearly_1960_2024.rds"))

kaplan <- imap(kaplan_offenses, \(column, group) {
  kaplan_raw |>
    filter(between(year, first_year, 1999)) |>
    transmute(
      year,
      ori,
      state_abbr = as.character(as.vector(state_abb)),
      group,
      reported = as.vector(.data[[str_c("actual_", column)]]),
      cleared = coalesce(as.vector(.data[[str_c("total_cleared_", column)]]), 0)
    ) |>
    as_tibble()
}) |>
  list_rbind()

nypd_clearance <- read_csv("data/nypd_clearance_2022.csv", show_col_types = FALSE)

cde <- read_rds(csg_sp_path("jr_data_library", "data", "analysis", "fbi", "srs", "fbi_srs_agency.rds")) |>
  filter(group %in% violent_offenses, indicator %in% c("Incidents reported", "Incidents cleared"), year < published_year) |>
  mutate(indicator = if_else(indicator == "Incidents reported", "reported", "cleared")) |>
  summarize(n = sum(n, na.rm = TRUE), .by = c(year, ori, state_abbr, group, indicator)) |>
  pivot_wider(names_from = indicator, values_from = n, values_fill = 0) |>
  mutate(across(c(state_abbr, group), as.character))

agency_offenses <- bind_rows(kaplan, cde) |>
  filter(state_abbr %in% c(state.abb, "DC"))

### Screen on violent totals --------------------------------------------------

### an agency is non-reporting when its violent total is at least
### min_screen_reported with no clearances
agency_totals <- agency_offenses |>
  summarize(across(c(reported, cleared), \(x) sum(x, na.rm = TRUE)), .by = c(year, ori, state_abbr)) |>
  filter(reported > 0) |>
  mutate(non_reporting = reported >= min_screen_reported & cleared == 0)

kept_state_years <- agency_totals |>
  summarize(share_no_clearances = sum(reported[non_reporting]) / sum(reported), .by = c(year, state_abbr)) |>
  filter(
    share_no_clearances < max_no_clearance_share,
    !(state_abbr == "IL" & year > illinois_last_year)
  ) |>
  select(year, state_abbr)

rebuilt <- agency_offenses |>
  semi_join(kept_state_years, by = join_by(year, state_abbr)) |>
  summarize(pct_solved = sum(cleared, na.rm = TRUE) / sum(reported, na.rm = TRUE), .by = c(year, group))

### Spliced series ------------------------------------------------------------

published <- us_solve_trend(read_srs_state(), published_year, by_offense = TRUE) |>
  select(year, group, pct_solved)

by_offense <- bind_rows(
  `site method from agency counts` = rebuilt,
  `published site method` = published,
  .id = "solve_method"
) |>
  mutate(group = factor(group, violent_offenses)) |>
  arrange(group, year)

if (any(count(by_offense, group)$n != 2025 - first_year + 1)) {
  stop("Every offense needs one solve rate per year from ", first_year, " to 2025")
}

write_csv(by_offense, "analysis/solve-rate-by-offense-since-1966.csv")

### lowest 15 years per offense
by_offense |>
  slice_min(pct_solved, n = 15, by = group, with_ties = FALSE) |>
  mutate(rank = row_number(), pct_solved = round(100 * pct_solved, 1), .by = group) |>
  select(group, rank, year, pct_solved) |>
  print(n = Inf)
