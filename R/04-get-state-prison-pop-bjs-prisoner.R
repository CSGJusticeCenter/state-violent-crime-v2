## read in and clean up state and us prison population data from bjs prisoner series

library(tidyverse)

# read in prisoners in 2020 bjs report with total state prison popuation
# downloaded from https://bjs.ojp.gov/library/publications/prisoners-2020-statistical-tables
# clean up state names and calculate total violent prison pop
# note we're going to use this for marhsall's ppt static graphs,
# not the interactive site where we use ncrp data
viol_prison_state <- read_csv("data/p20stt16.csv", skip = 10) |>
  select(
    state = 2,
    total_pop = 3,
    violent_pct = 6
  ) |>
  filter(!is.na(state)) |>
  mutate(
    state = str_remove(state, "/.*"),
    violent_pct = violent_pct / 100,
    violent_n = total_pop * violent_pct
  )

# read in csv with us total prison pop and viol pop
# manually created from data in bjs prisoners in us series reports
# e.g. https://bjs.ojp.gov/library/publications/prisoners-2020-statistical-tables
# differs year to year which table has these counts
# note we're going to use this for marhsall's ppt static graphs,
# not the interactive site where we use ncrp data
us_prison <- read_csv("data/us-prison-pop-violent.csv")

# write to disk
write_rds(us_prison, "data/us_prison.rds")
write_rds(viol_prison_state, "data/viol_prison_state.rds")
