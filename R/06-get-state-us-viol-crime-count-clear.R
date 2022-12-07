## get and clean up state and us violent crime counts and clearance rates
## from a variety of sources: srs estimated crime, srs agency-level offenses known
## fbi crime in the us

library(tidyverse)

# path to data folder on sharepoint
sp_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")

# read in state population
state_pop <- read_rds(file.path(sp_path, "state_pop.rds")) |>
  filter(state_abb != "PR")

# calculate total us pop by year by aggregating state pop ests
us_pop <- state_pop |>
  group_by(year) |>
  summarize(across(c(pop_total, pop_adult), sum))

# read in state-level estimates of index crime from fbi
# downloaded from https://crime-data-explorer.fr.cloud.gov/pages/downloads
# scroll down to additional datasets > summary reporting system
est_crime_by_state <- read_csv(file.path(sp_path, "estimated_crimes_1979_2020.csv"))

# read in ucr srs offenses known
# jacob kaplan's concatenated files - of agency-level annual crime data reported to fbi
# download from https://www.openicpsr.org/openicpsr/project/100707/version/V17/view
okca <- read_rds(file.path(sp_path, "offenses_known_yearly_1960_2020.rds"))

# read in national violent crime counts and clearance rates
# one file is all viol crime, other by offense
# these come from fbi crime in the us, table 25a
# for each year, i manually created a csv file from the annual reports
# e.g. https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/tables/table-25
# for 2020, crime in the us was only published on fbi cde
# https://crime-data-explorer.fr.cloud.gov/pages/downloads
# scroll to Crime in the United States Annual Reports and download
us_viol_crime <- read_csv(file.path(sp_path, "us-crime-clear-cius.csv"))
us_viol_crime_by_off <- read_csv(file.path(sp_path, "us-crime-clear-cius-by-off.csv"))

# clean up state crime estimates
# only take 2010 to 2020, keep violent index crimes only
# combine revised rape and legacy rape into one (change to revised in 2013)
# reshape to long with one row per yer per state per crime
est_crime_by_off_state <- est_crime_by_state |>
  filter(year >= 2010, !is.na(state_name)) |>
  transmute(
    year,
    state = state_name,
    homicide,
    rape = coalesce(rape_revised, rape_legacy),
    robbery,
    aggravated_assault
  ) |>
  pivot_longer(
    -c(year, state),
    names_to = "crime",
    values_to = "actual_est"
  ) |>
  mutate(
    crime = str_replace(crime, "_", " "),
    crime = str_to_sentence(crime)
  )

# were going to take the agency-level srs crime data and aggregate up to state level
# first clean up the names of the crimes we are interested in
# and combine murder with manslaughter to get a total homicide count
# we will take 'actual' which is num of reported crimes and 'cleared' which is crimes cleared
# reshape to long so that we can group by state, year, crime, and get a state aggregate number
# calculate clearance rate (cleared / actual) and reformat crime names
# join with state population data and fbi state-level estimates
# (which differ slightly from srs aggregated due to non-reporting in srs)
# finally we get a data set with one row per state per year per crime with actual,
# cleared, clearance rate, actual estimated, and state pop estimates
viol_crime_by_off_state <- okca |>
  as_tibble() |>
  filter(year >= 2010, !is.na(state_abb)) |>
  transmute(
    ori9,
    crosswalk_agency_name,
    state_abb,
    year,
    population_group,
    population,
    actual_homicide = actual_murder + actual_manslaughter,
    actual_rape = actual_rape_total,
    actual_robbery = actual_robbery_total,
    actual_aggravated_assault = actual_assault_aggravated,
    cleared_homicide = tot_clr_murder + tot_clr_manslaughter,
    cleared_rape = tot_clr_rape_total,
    cleared_robbery = tot_clr_robbery_total,
    cleared_aggravated_assault = tot_clr_assault_aggravated,
    ) |>
  pivot_longer(c(starts_with("actual"), starts_with("clear")), values_to = "n") |>
  group_by(state_abb, year, name) |>
  summarize(n = sum(n)) |>
  ungroup() |>
  separate(name, into = c("status", "crime"), sep = "_", extra = "merge") |>
  pivot_wider(names_from = status, values_from = n) |>
  mutate(
    clearance_rate = cleared / actual,
    crime = str_replace(crime, "_", " "),
    crime = str_to_sentence(crime)
    ) |>
  left_join(state_pop, by = c("state_abb", "year")) |>
  left_join(est_crime_by_off_state, by = c("year", "state", "crime")) |>
  mutate(cleared_est = actual_est * clearance_rate)

# using above dataset, aggregate up to all violent crime by adding the four index crimes
# calculated clearance rate after aggregation
viol_crime_by_state <- viol_crime_by_off_state |>
  group_by(state, state_abb, year, pop_total, pop_adult) |>
  summarize(across(c(actual, cleared, actual_est), sum)) |>
  ungroup() |>
  mutate(
    clearance_rate = cleared / actual,
    cleared_est = actual_est * clearance_rate
    )

# from crime in us estimates, join us pop and calculate cleared from actual and clearance rate
# note we are creating columns called actual and actual_est that are the same
# because we probably will use the actual_est column at the state-level from above when plotting
# which differs from actual - but at us level we only have this one crime estimate
viol_crime_us <- us_viol_crime |>
  left_join(us_pop, by = "year") |>
  mutate(
    actual_est = actual,
    cleared = actual * clearance_rate,
    cleared_est = cleared
    )

# do the same by offense
viol_crime_by_off_us <- us_viol_crime_by_off |>
  left_join(us_pop, by = "year") |>
  mutate(
    actual_est = actual,
    cleared = actual * clearance_rate,
    cleared_est = actual_est * clearance_rate
  )

# write to disk
write_rds(viol_crime_by_off_state, file.path(sp_path, "viol_crime_by_off_state.rds"))
write_rds(viol_crime_by_state, file.path(sp_path, "viol_crime_by_state.rds"))
write_rds(viol_crime_by_off_us, file.path(sp_path, "viol_crime_by_off_us.rds"))
write_rds(viol_crime_us, file.path(sp_path, "viol_crime_us.rds"))
