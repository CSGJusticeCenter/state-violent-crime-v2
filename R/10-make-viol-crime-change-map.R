####Develop the following for Marshall
#1. Table with states with >90% NIBRS participation: # of states with increase & # of states with decrease in each type of violent crime in 2021
#2. Table with states with >90% NIBRS participation: # of states with increase in clearance & # of states with decrease in clearance for each type of violent crime in 2021
#3. Four line graphs that show violent crime rates over time by crime (the current viz is a stacked line chart - he wants them separately) for states with >90% NIBRS participation

library(tidyverse)
library(csgjcr)
library(highcharter)
library(htmlwidgets)

source("R/utils.R")

# path to data folder on sharepoint
sp_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")

# read in crime data
viol_crime_by_state          <- read_rds(file.path(sp_path, "viol_crime_by_state.rds")) |>
  filter(!is.na(state))
viol_crime_by_off_state      <- read_rds(file.path(sp_path, "viol_crime_by_off_state.rds")) |>
  filter(!is.na(state))
viol_crime_2021_state        <- read_rds(file.path(sp_path, "viol_crime_2021_state.rds"))
viol_crime_2021_by_off_state <- read_rds(file.path(sp_path, "viol_crime_2021_by_off_state.rds"))

hex <- sf::read_sf("us_states_hexgrid.geojson") |>
  select(state_abb = iso3166_2)

hex_gj <- hex |>
  sf::st_transform(3857) |>
  geojsonsf::sf_geojson() |>
  jsonlite::fromJSON(simplifyVector = FALSE)

to_map <- viol_crime_2021_state |>
  filter(pop_cov >= 0.9) |>
  transmute(state, viol_crime_21 = actual, viol_crime_rate_21 = actual / pop_total) |>
  full_join(filter(viol_crime_by_state, year == 2019), by = "state") |>
  transmute(state, state_abb, viol_crime_21, viol_crime_rate_21,
            viol_crime_19 = actual, viol_crime_19_rate = actual / pop_total) |>
  mutate(
    change = viol_crime_21 - viol_crime_19,
    pct_change = change / viol_crime_19,
    status = case_when(
      pct_change > 0 ~ 1,
      pct_change <= 0 ~ 2,
      is.na(pct_change) ~ 3,
    ),
    url = paste0("https://projects.csgjusticecenter.org/50-states-crime-report/50-state-crime-data/?state=", tolower(state_abb))

  )

map_colors <- list(
  list(name = "Increase",     from = 1, to = 1, color = jr_pal[2]),
  list(name = "Decrease",     from = 2, to = 2, color = jr_pal[3]),
  list(name = "No 2021 data", from = 3, to = 3, color = "darkgray")
)

hc <- highchart()  |>
  hc_add_series_map(
    map = hex_gj,
    df = to_map,
    joinBy = "state_abb",
    value = "status",
    dataLabels = list(enabled = TRUE, format = "{point.state_abb}",
                      style = list(fontSize = "13px",
                                   fontFamily = "GT-America",
                                   fontWeight = 700,
                                   textOutline = 0))
  )  |>
  hc_colorAxis(
    dataClassColor = "category",
    dataClasses = map_colors
  ) |>
  hc_legend(
    itemStyle = list(fontSize = "14px",
                     fontFamily = "GT-America",
                     fontWeight = 400)
  ) |>
  hc_tooltip(enabled = FALSE) |>
  hc_plotOptions(
    series = list(
      animation = FALSE,
      states = list(inactive = list(opacity = 1)),
      cursor = "pointer",
      accessibility = list(
        enabled = TRUE,
        keyboardNavigation = list(enabled = TRUE)
      ),
      point = list(
        events = list(
          click = JS("function(){location.href = this.options.url}"))
      )
    )
  ) |>
  hc_add_dependency(name = "modules/accessibility.js")

saveWidget(hc, "hex-map.html", selfcontained = FALSE, libdir = "map-libs")
