## Fact check: "The 2020 to 2025 period produced more unsolved murders than any
## prior six-year period since data was first collected in 1960."
##
## Covers 1985-2025, the years with FBI-estimated clearances for every state.
## Unsolved murders are estimated murders minus estimated clearances. A year's
## clearances can include murders from earlier years.

library(tidyverse)
library(csgjcr)
library(scales)
source("R/page-data.R")

first_year <- 1985
window <- 6
claim_start <- 2020

srs <- read_srs_state() |>
  filter(group == "Homicide", year >= first_year)

### state-years without clearances (FL and KY 1988, and a few small states)
### would count as fully solved, so they leave the pooled rate like screened rows
missing_clearances <- filter(srs, is.na(incidents_unsolved))
srs <- mutate(srs, hide_offense_solve = hide_offense_solve | is.na(incidents_unsolved))

### the site's method, which repairs NYPD's 2022 clearances and applies one
### pooled rate, leaving out Illinois and screened state-years, to every state
site <- srs |>
  us_solve_trend(first_year, by_offense = TRUE) |>
  select(year, murders = incidents_reported, unsolved = incidents_unsolved, pct_cleared = pct_solved)

### check against each state's own clearances, which include Illinois's low
### reported clearances. State-years without clearances drop out
state_sum <- srs |>
  filter(!is.na(incidents_unsolved)) |>
  summarize(state_sum_unsolved = sum(incidents_unsolved), .by = year)

annual <- left_join(site, state_sum, by = join_by(year), relationship = "one-to-one")

### sum of each measure over every window-year span
window_sums <- function(annual, n = window) {
  starts <- min(annual$year):(max(annual$year) - n + 1)
  map(starts, \(start) {
    annual |>
      filter(between(year, start, start + n - 1)) |>
      summarize(
        period = str_c(start, "-", start + n - 1),
        start = start,
        murders = sum(murders),
        unsolved = sum(unsolved),
        state_sum_unsolved = sum(state_sum_unsolved)
      )
  }) |>
    list_rbind() |>
    mutate(rank = min_rank(desc(unsolved)))
}

windows <- window_sums(annual)

missing_clearances |>
  select(year, state_abbr, incidents_reported) |>
  print()

print(annual, n = Inf, width = Inf)

windows |>
  arrange(rank) |>
  print(n = Inf, width = Inf)

### the claim window against every window, and against windows that end before it
claim <- filter(windows, start == claim_start)
top_any <- slice_max(filter(windows, start != claim_start), unsolved, n = 1)
top_prior <- slice_max(filter(windows, start <= claim_start - window), unsolved, n = 1)
message(
  claim$period, ": ", comma(claim$unsolved), " unsolved murders, rank ", claim$rank, " of ", nrow(windows), "\n",
  "Most of any other window: ", top_any$period, ", ", comma(top_any$unsolved), "\n",
  "Most of windows ending before ", claim_start, ": ", top_prior$period, ", ", comma(top_prior$unsolved)
)
