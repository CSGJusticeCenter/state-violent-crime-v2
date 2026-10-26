## Every number in the Crime Without Consequence handout, pages 1-4
##
## Each section quotes the handout text and computes its numbers. The script
## prints one table of them at the end. Reads the CSVs from 01-national-series.R
## and 02-state-2020-2025.R. Run from the project root.

library(tidyverse)
library(csgjcr)
library(scales)
source("R/page-data.R")
source("analysis/crime_wo_conseq/helpers.R")

national <- read_national()
states <- read_states()

recent <- recent_years(national)
reported_states <- filter(states, !no_data)

### Page 1 ---------------------------------------------------------------------

### "Over 4.3 MILLION violent crimes went unsolved between 2020 and 2025"
unsolved_2020_2025 <- sum(recent$incidents_unsolved)

### "less than half of new crimes getting solved"
pct_solved_2020_2025 <- pooled_solve_rate(recent)

### "Nearly 49,000 homicides" and "Also unsolved 2020–2025 (approx.)" by offense.
### Each offense has its own pooled US solve rate, applied to that offense's
### reported count
offense_unsolved <- us_solve_trend(read_srs_state(), 2020, by_offense = TRUE) |>
  filter(year <= 2025) |>
  summarize(unsolved = sum(incidents_unsolved), .by = group) |>
  pull(unsolved, name = group)

### "Violent Crime Reported, Solved, and Unsolved in the U.S., 2020–2025" and
### "Backlog of Unsolved Violent Crime, 2020–2025"
yearly <- recent |>
  transmute(
    year,
    reported = incidents_reported,
    solved = incidents_solved,
    unsolved = incidents_unsolved,
    backlog = cumsum(incidents_unsolved)
  )

### Page 2 ---------------------------------------------------------------------

### "Solve rates were below 50% in 30 states and DC from 2020 to 2025"
below_half <- filter(reported_states, pct_solved < 0.5)
n_states_below_half <- sum(below_half$state_abbr != "DC")
dc_below_half <- "DC" %in% below_half$state_abbr

### "Massachusetts, Oregon, and Pennsylvania round to 50% but are just over half,"
### so they are not among the states below 50%
round_to_half <- filter(reported_states, round(100 * pct_solved) == 50, pct_solved >= 0.5)$state_name

### map shares and the state table
state_table <- states |>
  transmute(
    state_name,
    solved = if_else(no_data, "---", solved_label(pct_solved, year_missing)),
    unsolved_violent = if_else(no_data, "No data", str_c(if_else(year_missing, "≥ ", ""), comma(unsolved, 1))),
    unsolved_homicides = if_else(no_data, "No data", str_c(if_else(homicide_year_missing, "≥ ", ""), comma(homicide_unsolved, 1)))
  )

### Page 3 ---------------------------------------------------------------------

### "Violent Crime Solved and Unsolved in the U.S., 1966–2025" five-year bars
five_year <- periods(filter(national, year >= 1966), 5)

### Solved counts before 2015 come from the agency-count rebuild and later ones
### from the published method, so the comparisons below cross that splice. The
### checks in 01-national-series.R print both rates for the overlap years
solved_in <- function(y) national$incidents_solved[national$year == y]

### "Law enforcement solved about 534,000 violent crimes in 2025, roughly
### 300,000 fewer than in the early 1990s." Early 1990s is the 1991-1995
### yearly average, the peak period in the chart
solved_2025 <- solved_in(2025)
solved_early_1990s <- five_year$incidents_solved[five_year$label == "1991–95"]
stopifnot(length(solved_early_1990s) == 1)
solved_drop <- solved_early_1990s - solved_2025

### "Since 2021, they've solved fewer per year than at any time since the late
### 1970s." Every year from 2021 on solved fewer than the fewest year from
### 1980 to 2020, and the fewest year from 2015 to 2020, all published method
solved_since_2021 <- national$incidents_solved[national$year >= 2021]
fewest_1980_2020 <- min(national$incidents_solved[between(national$year, 1980, 2020)])
fewest_2015_2020 <- min(national$incidents_solved[between(national$year, 2015, 2020)])
stopifnot(max(solved_since_2021) < fewest_1980_2020)

### last year before 2021 with fewer solved than the most since 2021, and with
### fewer than every year since 2021
last_year_below_most <- max(national$year[national$year < 2021 & national$incidents_solved < max(solved_since_2021)])
last_year_below_all <- max(national$year[national$year < 2021 & national$incidents_solved < min(solved_since_2021)])

### "The lowest solve rates on record were from 2021 through 2023"
lowest_rates <- slice_min(national, pct_solved, n = 3)

### "State and local spending on law enforcement rose 25% after inflation from
### 2014 to 2024." Census ASSLGF police protection, in 2024 dollars
spending <- read_police_spending("US", read_inflation_index())
spending_at <- function(y) spending$n_inflation_adjusted_latest_year_dollars[spending$year == y]
spending_change <- spending_at(2024) / spending_at(2014) - 1

### "More than 771,000 violent crimes went unsolved in 2022 alone"
unsolved_2022 <- national$incidents_unsolved[national$year == 2022]

### Page 4 ---------------------------------------------------------------------

### "U.S. rate" line: reported crimes over population summed across years. The
### state sums and the national series give the same rate
us_crime_rate <- pooled_crime_rate(states)
stopifnot(all.equal(us_crime_rate, sum(recent$incidents_reported) / sum(recent$population) * 1e5))

### "U.S. solve rate" line: the national series, as on pages 1 and 3
us_solve_rate <- pct_solved_2020_2025

### alt text: Mississippi and Pennsylvania under 300 crimes per 100,000;
### Vermont solved the most and New Mexico the least
state_points <- reported_states |>
  select(state_name, reported_per_100k, pct_solved) |>
  arrange(pct_solved)

### "unsolved counts for the four offenses add up to slightly more than the
### total for all violent crime"
offense_sum_gap <- sum(offense_unsolved) - unsolved_2020_2025

### All numbers ----------------------------------------------------------------

state_value <- function(state, column) state_points[[column]][state_points$state_name == state]

numbers <- tribble(
  ~page, ~claim, ~value,
  1, "Violent crimes unsolved, 2020-2025", comma(unsolved_2020_2025),
  1, "Share solved, 2020-2025", percent(pct_solved_2020_2025, 0.1),
  1, "Homicides unsolved, 2020-2025", comma(offense_unsolved[["Homicide"]]),
  1, "Aggravated assaults unsolved, 2020-2025", comma(offense_unsolved[["Aggravated assault"]]),
  1, "Robberies unsolved, 2020-2025", comma(offense_unsolved[["Robbery"]]),
  1, "Rapes unsolved, 2020-2025", comma(offense_unsolved[["Rape"]]),
  1, "Reported by year", str_flatten(comma(yearly$reported), ", "),
  1, "Solved by year", str_flatten(comma(yearly$solved), ", "),
  1, "Backlog by year", str_flatten(comma(yearly$backlog), ", "),
  2, "States below 50% solved", str_c(n_states_below_half, if (dc_below_half) " and DC" else ""),
  2, "Round to 50% but just over half solved", str_flatten_comma(round_to_half),
  3, "Five-year solve rates", str_flatten(str_c(five_year$label, " ", percent(five_year$pct_solved, 1)), ", "),
  3, "Peak five-year average reported", str_c(comma(max(five_year$incidents_reported)), " (", five_year$label[which.max(five_year$incidents_reported)], ")"),
  3, "Solved in 2014 (rebuilt) and 2015 (published)", str_c(comma(solved_in(2014)), ", ", comma(solved_in(2015))),
  3, "Solved in 2025", comma(solved_2025),
  3, "Solved per year, 1991-95, less 2025", comma(solved_drop),
  3, "Most solved in a year since 2021", comma(max(solved_since_2021)),
  3, "Fewest solved in a year, 1980-2020", comma(fewest_1980_2020),
  3, "Fewest solved in a year, 2015-2020", comma(fewest_2015_2020),
  3, "Last year before 2021 with fewer solved than the most since 2021", as.character(last_year_below_most),
  3, "Last year before 2021 with fewer solved than every year since 2021", as.character(last_year_below_all),
  3, "Lowest solve rates", str_flatten(str_c(lowest_rates$year, " ", percent(lowest_rates$pct_solved, 0.1)), ", "),
  3, "Police spending change after inflation, 2014-2024", percent(spending_change, 0.1),
  3, "Unsolved in 2022", comma(unsolved_2022),
  4, "U.S. crime rate per 100,000", comma(us_crime_rate, 0.1),
  4, "U.S. solve rate", percent(us_solve_rate, 0.1),
  4, "Mississippi rate and solved", str_c(comma(state_value("Mississippi", "reported_per_100k")), ", ", percent(state_value("Mississippi", "pct_solved"))),
  4, "Pennsylvania rate and solved", str_c(comma(state_value("Pennsylvania", "reported_per_100k")), ", ", percent(state_value("Pennsylvania", "pct_solved"))),
  4, "Most solved", str_c(last(state_points$state_name), " ", percent(last(state_points$pct_solved))),
  4, "Least solved", str_c(first(state_points$state_name), " ", percent(first(state_points$pct_solved))),
  4, "Offense unsolved sum less violent total", comma(offense_sum_gap)
)

writeLines(str_glue("p{numbers$page}  {numbers$claim}: {numbers$value}"))
print(state_table, n = Inf)
