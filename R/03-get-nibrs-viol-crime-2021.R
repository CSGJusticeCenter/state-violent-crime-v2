## get 2021 state-level estimates of violent crime and clearnce from nibrs estimation files
## https://crime-data-explorer.fr.cloud.gov/pages/downloads#nibrsestimationDownloads

library(tidyverse)

# path to data folder on sharepoint
sp_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")

# read in state population
state_pop <- read_rds(file.path(sp_path, "state_pop.rds")) |>
  filter(state_abb != "PR")

# make vector of all state nibrs estimatation files
nibrs_state_files <- file.path(sp_path, paste0("nibrs_estimation_files/Indicator_Tables_no_supp_no_LEOKA_",56:106, ".csv"))

# read in and bind all state nibrs estimates
# this is a large and hard to use/understand set of files with limited documentation :(
# but essentially we just need to find state-level estimated for a few indicators that we need
nibrs_est <- read_csv(nibrs_state_files)

# these are the variable names that are the 4 violent index crime incident counts and total violent crime
viol_inc_count <- c("t_1a_1_1_2", "t_1a_1_1_5", "t_1a_1_1_15", "t_1a_1_1_16", "t_1a_1_1_17")

# these are the variable names that are the 4 violent index crime and not violent crime not cleared percentages
viol_inc_clear <- c("t_1a_13_64_2", "t_1a_13_64_5", "t_1a_13_64_15", "t_1a_13_64_16", "t_1a_13_64_17")

# subset to just the incident counts and not cleared percentage
# for our 4 viol index crimes and total viol crime
nibrs_viol_count_clear <- nibrs_est |>
  filter(
    der_variable_name %in% viol_inc_count |
      (der_variable_name %in% viol_inc_clear & estimate_type == "percentage")
  ) |>
  select(
    indicator_name,
    estimate_domain_1,
    estimate_domain_2,
    estimate_geographic_location,
    estimate,
    estimate_upper_bound,
    estimate_lower_bound,
    pop_cov,
    agency_counts
  )

# do some very ugly reshaping to create a data set with one row per crime per state per year
# and counts of actual incidents, clearance rates, cleared incidents
# with upper and lower bounds and moes
# rename crimes to align with ucr offense categories
viol_crime_2021_state <- nibrs_viol_count_clear |>
  group_by(
    year = 2021,
    state = str_remove(estimate_geographic_location, "State "),
    crime = indicator_name
    ) |>
  summarize(
    actual = estimate[estimate_domain_1 == "Incident count"],
    actual_est = actual,
    actual_upper = estimate_upper_bound[estimate_domain_1 == "Incident count"],
    actual_lower = estimate_lower_bound[estimate_domain_1 == "Incident count"],
    actual_moe = actual_upper - actual,
    clearance_rate = (100 - estimate[estimate_domain_1 == "Clearance"]) / 100,
    clearance_rate_upper = (100 - estimate_upper_bound[estimate_domain_1 == "Clearance"]) / 100,
    clearance_rate_lower = (100 - estimate_lower_bound[estimate_domain_1 == "Clearance"]) / 100,
    clearance_rate_moe = clearance_rate_upper - clearance_rate,
    cleared = actual * clearance_rate,
    cleared_est = cleared,
    agency_counts = agency_counts[estimate_domain_1 == "Incident count"],
    pop_cov = pop_cov[estimate_domain_1 == "Incident count"]
  ) |>
  ungroup() |>
  mutate(
    crime = case_when(
      crime == "Murder and Non-negligent Manslaughter" ~ "Homicide",
      crime == "Aggravated Assault"                    ~ "Aggravated assault",
      crime == "Revised Rape"                          ~ "Rape",
      crime == "Violent Crime"                         ~ "Total violent crime"
      )
    ) |>
  left_join(state_pop, by = c("state", "year"))

# write two files - first with each of the 4 offenses
viol_crime_2021_state |>
  filter(crime != "Total violent crime") |>
  write_rds(file.path(sp_path, "viol_crime_2021_by_off_state.rds"))

# second with just total violent crime
viol_crime_2021_state |>
  filter(crime == "Total violent crime") |>
  write_rds(file.path(sp_path, "viol_crime_2021_state.rds"))



# not used at this time
# nibrs_us <- paste0("data/Indicator_Tables_no_supp_no_LEOKA/Indicator_Tables_no_supp_no_LEOKA_01.csv")
#
# h <- read_csv("data/Indicator_Tables_no_supp_no_LEOKA/Indicator_Tables_no_supp_no_LEOKA_1.csv")
#
# h |>
#   filter(
#     der_variable_name %in% off_count |
#       (der_variable_name %in% off_clear & estimate_type == "percentage")
#   ) |>
#   select(
#     indicator_name,
#     estimate_domain_1,
#     estimate_domain_2,
#     estimate_geographic_location,
#     estimate,
#     estimate_upper_bound,
#     estimate_lower_bound,
#     pop_cov,
#     agency_counts
#   ) |> write_csv("a.csv")
#
#
#
#
# b <- nibrs_est |>
#   filter(
#     der_variable_name == "t_1a_1_1_17" |
#       (der_variable_name == "t_1a_13_64_17" & estimate_type == "percentage")
#   ) |>
#   select(
#     indicator_name,
#     estimate_domain_1,
#     estimate_domain_2,
#     estimate_geographic_location,
#     estimate,
#     estimate_upper_bound,
#     estimate_lower_bound,
#     pop_cov,
#     agency_counts
#   )
#
# viol_crime_2021_state <- b |>
#   group_by(state = str_remove(estimate_geographic_location, "State ")) |>
#   summarize(
#     point_violent_crime = estimate[estimate_domain_1 == "Incident count"],
#     upper_violent_crime = estimate_upper_bound[estimate_domain_1 == "Incident count"],
#     lower_violent_crime = estimate_lower_bound[estimate_domain_1 == "Incident count"],
#     point_violent_crime_clearance_rate = sum(estimate[estimate_domain_1 == "Clearance"]) / 100,
#     upper_violent_crime_clearance_rate = sum(estimate_upper_bound[estimate_domain_1 == "Clearance"]) / 100,
#     lower_violent_crime_clearance_rate = sum(estimate_lower_bound[estimate_domain_1 == "Clearance"]) / 100,
#     agency_counts = agency_counts[estimate_domain_1 == "Incident count"],
#     pop_cov = pop_cov[estimate_domain_1 == "Incident count"]
#   ) |>
#   ungroup() |>
#   pivot_longer(-c(state, pop_cov, agency_counts)) |>
#   separate(name, into = c("type", "crime"), sep = "_", extra = "merge") |>
#   pivot_wider(names_from = type, values_from = value)
