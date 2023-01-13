## read in and clean prison population by state from bjs csv files
## these files have all sorts of quirks and you'll see a number year/state specific
## special cases in the cleaning

library(tidyverse)
library(csgjcr)
library(glue)

# path to data folder on sharepoint
sp_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")
prison_path <- file.path(sp_path, "bjs_prisoners")

# function to read in bjs csv files, clean and come out with
# state name and total prison pop as published
read_clean_bjs_prison_1 <- function(year) {

  year_two <- str_sub(year, 3)
  file_name <- glue("p{year_two}at01.csv")

  read_csv(
    file.path(prison_path, glue("p{year_two}"), file_name),
    skip = 9,
    col_select = c(2, 5)
  ) |>
    rename(state = 1, n = 2) |>
    mutate(
      state = str_remove(state, "/.*"),
      state = if_else(state == "Alaskab", "Alaska", state),
      n = parse_number(as.character(n)),
      year = year
    ) |>
    filter(state %in% state.name)
}

# works only for 2010 and 2011
a <- map_dfr(2010:2011, read_clean_bjs_prison_1)

# a slightly different function to read in bjs csv files, clean and come out with
# state name and total prison pop as published
read_clean_bjs_prison_2 <- function(year) {

  year_two <- str_sub(year, 3)

  file_name <- case_when(
    year == 2012 ~ glue("p12tar9112at06.csv"),
    year > 2012 ~ glue("p{year_two}t02.csv")
  )

  read_csv(
    file.path(prison_path, glue("p{year_two}"), file_name),
    skip = 11,
    col_select = c(2, 7)
  ) |>
    rename(state = 1, n = 2) |>
    mutate(
      state = str_remove(state, "/.*"),
      state = case_when(
        state == "Utahc" ~ "Utah",
        state == "Wisconsing" ~ "Wisconsin",
        state == "Idah" ~ "Idaho",
        TRUE ~ state
      ),
      n = parse_number(as.character(n)),
      year = year
    ) |>
    filter(state %in% state.name)
}

# works for 2012-2019
b <- map_dfr(2012:2019, read_clean_bjs_prison_2)


# 2020 is it's own special snowflake of data cleaning
c <- read_csv(
  file.path(prison_path, "p20st", "p20stt02.csv"),
  skip = 11,
  col_select = c(2, 6)
) |>
  rename(state = 1, n = 2) |>
  mutate(
    state = str_remove(state, "/.*"),
    n = parse_number(as.character(n)),
    year = 2020
  ) |>
  filter(state %in% state.name)

# combine each cleaned year(s) of data
prison_by_state <- bind_rows(a, b, c) |>
  arrange(year, state) |>
  rename(tot_prison_pop = n)

# write to disk
write_rds(prison_by_state, file.path(prison_path, "prison_by_state.rds"))
