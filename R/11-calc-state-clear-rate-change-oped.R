library(tidyverse)
library(csgjcr)

source("R/utils.R")

# path to data folder on sharepoint
sp_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")

# read in crime data
viol_crime_by_state <- read_rds(file.path(sp_path, "viol_crime_by_state.rds"))
viol_crime_by_off_state <- read_rds(file.path(sp_path, "viol_crime_by_off_state.rds"))

viol_crime_2021_state <- read_rds(file.path(sp_path, "viol_crime_2021_state.rds")) |>
  filter(pop_cov >= 0.9)
viol_crime_2021_by_off_state <- read_rds(file.path(sp_path, "viol_crime_2021_by_off_state.rds")) |>
  filter(pop_cov >= 0.9)

viol_crime_by_off_state <- viol_crime_by_off_state |>
    bind_rows(viol_crime_2021_by_off_state)

viol_crime_by_state <- viol_crime_by_state |>
    bind_rows(viol_crime_2021_state)


viol_crime_by_off_state |>
  filter(crime == "Homicide", !is.na(state), state != "Illinois") |>
  group_by(state) |>
  filter(year %in% c(2011, max(year))) |>
  mutate(
    start_end = if_else(year == 2011, "clearance_start", "clearance_end"),
    startyear = 2011,
    endyear = max(year)
    ) |>
  ungroup() |>
  select(state, start_end, startyear, endyear, clearance_rate) |>
  pivot_wider(names_from = start_end, values_from = clearance_rate) |>
  mutate(change = clearance_end - clearance_start) |>
  write_csv("change in homicide clearance rate.csv")
