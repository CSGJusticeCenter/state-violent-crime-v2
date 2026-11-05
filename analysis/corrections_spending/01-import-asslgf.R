## The full Census Annual Survey of State and Local Government Finances
## (ASSLGF), 2017-2024: every item, state and level of government, for
## exploring corrections spending. The site's census_asslgf_state.rds keeps
## only state and local totals for police, corrections and judicial spending.
## Run from the project root.

library(tidyverse)
source("R/page-data.R")

asslgf_path <- jr_data_path("data", "raw", "census", "asslgf", "GOVTIMESERIES",
                            "GS00LOCALFIN-2017_2024-20260928-Data.csv")

### one row per year, state, level of government and item. The file's second
### row repeats the column labels and a trailing comma adds an empty column.
### Item codes were renumbered in the 2024 release, so match items on labels
asslgf <- read_csv(asslgf_path, col_types = cols(.default = "c"), name_repair = "unique_quiet") |>
  slice(-1) |>
  transmute(
    year = as.integer(time),
    geo_name = NAME,
    geo_abbr = if_else(geo_name == "United States", "US",
                       csgjcr::csg_state_convert(geo_name, from = "name", "abbr")),
    gov_type = GOVTYPE_LABEL,
    item_code = AGG_DESC,
    item_label = AGG_DESC_LABEL,
    section = str_extract(item_label, "^[^-]+") |> str_trim(),
    item = str_extract(item_label, "[^-]+$") |> str_trim(),
    ### Census reports amounts in thousands of dollars
    amount = parse_number(AMOUNT) * 1000,
    ### "S" marks a suppressed coefficient of variation and "N" one not available
    amount_cv = parse_number(AMOUNT_CV, na = c("", "S", "N"))
  )

### corrections direct expenditure: total, current operations and capital outlay
corrections <- asslgf |>
  filter(str_detect(item_label, "General Expenditure - Correction - ")) |>
  select(year, geo_name, geo_abbr, gov_type, item, amount, amount_cv)
