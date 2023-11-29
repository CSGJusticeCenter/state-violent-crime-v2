## we're taking total prison population counts from bjs prisoner series report csvs
## and combining this with the proportions of violent vs non violent
## and violent offense types from ncrp to impute the total counts of prison pop
## by violent vs non violent and violent offense detail
## these are the final counts we will use in the state data pages

library(tidyverse)
library(csgjcr)

# path to data folder on sharepoint
sp_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")

# read in ncrp and bjs data
viol_prison_pop_state <- read_rds(file.path(sp_path, "viol_prison_pop_state.rds"))
viol_prison_pop_by_off_state <- read_rds(file.path(sp_path, "viol_prison_pop_by_off_state.rds"))
prison_by_state_bjs <- read_rds(file.path(sp_path, "bjs_prisoners", "prison_by_state.rds"))

# join ncrp viol/non-viol by year and state with bjs total pop
# use bjs total pop to impute violent and non-violent counts based on ncrp pct
# note, the total pop in bjs reports is usually a couple thousand higher than ncrp
viol_prison_pop_state_imputed <- viol_prison_pop_state |>
  left_join(prison_by_state_bjs, by = c("year", "state")) |>
  mutate(n_imputed = tot_prison_pop * pct) |>
  rename(n_ncrp = n, n = n_imputed) |>
  filter(!(state == "Virginia" & year == 2010))

# get impute violent population for each year and state
violent_imputed_to_join <- viol_prison_pop_state_imputed |>
  filter(violent_off == "Violent") |>
  select(state, year, viol_pop_imputed = n)

# join ncrp viol offense type by year and state with bjs imputed violent pop
# use bjs imputed violent pop to impute counts by offense type based on ncrp pct
viol_prison_pop_by_off_state_imputed <- viol_prison_pop_by_off_state |>
  left_join(violent_imputed_to_join, by = c("year", "state")) |>
  mutate(n_imputed = viol_pop_imputed * pct) |>
  rename(n_ncrp = n, n = n_imputed)

# write imputed prison pop to disk
write_rds(viol_prison_pop_state_imputed, file.path(sp_path, "viol_prison_pop_state_imputed.rds"))
write_rds(viol_prison_pop_by_off_state_imputed, file.path(sp_path, "viol_prison_pop_by_off_state_imputed.rds"))

