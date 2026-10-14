## compare Chicago PD's FBI counts and clearances with the city's own incident data
## the city data has an arrest flag but no clearance field

library(tidyverse)
library(csgjcr)

### city incidents by year and FBI code, with arrest-flag counts
url <- paste0(
  "https://data.cityofchicago.org/resource/ijzp-q8t2.csv?",
  "$select=year,fbi_code,count(*)%20as%20n,sum(case(arrest,1,true,0))%20as%20arrests",
  "&$where=year%20between%202018%20and%202025%20AND%20fbi_code%20in('01A','02','03','04A','04B')",
  "&$group=year,fbi_code&$limit=500"
)

city <- read_csv(url, show_col_types = FALSE) |>
  mutate(group = recode(fbi_code, `01A` = "Homicide", `02` = "Rape", `03` = "Robbery",
                        `04A` = "Aggravated assault", `04B` = "Aggravated assault")) |>
  summarize(city_reported = sum(n), city_arrests = sum(arrests), .by = c(year, group))

fbi <- read_rds(csg_sp_path("jr_data_library", "data", "analysis", "fbi", "srs", "fbi_srs_agency.rds")) |>
  filter(ori == "ILCPD0000", year >= 2018, group %in% c("Homicide", "Rape", "Robbery", "Aggravated assault")) |>
  select(year, group, indicator, n, months = n_months_reported) |>
  pivot_wider(names_from = indicator, values_from = n) |>
  janitor::clean_names() |>
  rename(fbi_reported = incidents_reported, fbi_cleared = incidents_cleared)

full_join(city, fbi, by = c("year", "group")) |>
  mutate(
    fbi_share_of_city = fbi_reported / city_reported,
    city_arrest_pct = city_arrests / city_reported,
    fbi_cleared_pct = fbi_cleared / fbi_reported
  ) |>
  arrange(group, year) |>
  print(n = Inf)
