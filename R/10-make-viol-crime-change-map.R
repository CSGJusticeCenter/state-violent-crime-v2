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

map_colors <- list(
  list(name = "Increase",     from = 1, to = 1, color = jr_pal[2]),
  list(name = "Decrease",     from = 2, to = 2, color = jr_pal[3]),
  list(name = "No 2021 data", from = 3, to = 3, color = "darkgray")
)

map_colors_rev <- list(
  list(name = "Increase",     from = 1, to = 1, color = jr_pal[1]),
  list(name = "Decrease",     from = 2, to = 2, color = jr_pal[4]),
  list(name = "No 2021 data", from = 3, to = 3, color = "darkgray")
)


make_hex_map <- function(df, colors) {
  highchart()  |>
    hc_add_series_map(
      map = hex_gj,
      df = df,
      joinBy = "state_abb",
      value = "status",
      dataLabels = list(enabled = TRUE, format = "{point.state_abb}",
                        style = list(fontSize = "13px",
                                     fontFamily = "GT America",
                                     fontWeight = 700,
                                     textOutline = 0))
    )  |>
    hc_colorAxis(
      dataClassColor = "category",
      dataClasses = colors
    ) |>
    hc_legend(
      itemStyle = list(fontSize = "14px",
                       fontFamily = "GT America",
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
            click = JS("function(){parent.document.location.href = this.options.url}"))
        )
      )
    ) |>
    hc_add_dependency(name = "modules/accessibility.js")
}

hex <- sf::read_sf("us_states_hexgrid.geojson") |>
  select(state_abb = iso3166_2)

hex_gj <- hex |>
  sf::st_transform(3857) |>
  geojsonsf::sf_geojson() |>
  jsonlite::fromJSON(simplifyVector = FALSE)

viol_crime_change <- viol_crime_2021_state |>
  filter(pop_cov >= 0.9) |>
  transmute(
    state,
    viol_crime_21 = actual,
    viol_crime_rate_21 = actual / pop_total,
    viol_crime_clear_21 = clearance_rate
    ) |>
  full_join(filter(viol_crime_by_state, year == 2019), by = "state") |>
  transmute(state, state_abb, viol_crime_21, viol_crime_rate_21, viol_crime_clear_21,
            viol_crime_19 = actual, viol_crime_rate_19 = actual / pop_total,
            viol_crime_clear_19 = clearance_rate)


to_map_viol_crime <- viol_crime_change |>
  mutate(
    change = viol_crime_rate_21 - viol_crime_rate_19,
    pct_change = change / viol_crime_rate_19,
    status = case_when(
      pct_change > 0 ~ 1,
      pct_change <= 0 ~ 2,
      is.na(pct_change) ~ 3,
    ),
    url = paste0("https://projects.csgjusticecenter.org/tools-for-states-to-address-crime/50-state-crime-data/?state=", tolower(state_abb))
  )

to_map_viol_crime_clear <- viol_crime_change |>
  mutate(
    change = viol_crime_clear_21 - viol_crime_clear_19,
    pct_change = change / viol_crime_clear_19,
    status = case_when(
      pct_change > 0 ~ 1,
      pct_change <= 0 ~ 2,
      is.na(pct_change) ~ 3,
    ),
    url = paste0("https://projects.csgjusticecenter.org/tools-for-states-to-address-crime/50-state-crime-data/?state=", tolower(state_abb))
  )

hom_change <- viol_crime_2021_by_off_state |>
  filter(pop_cov >= 0.9, crime == "Homicide") |>
  transmute(
    state,
    hom_21 = actual,
    hom_rate_21 = actual / pop_total,
    hom_clear_21 = clearance_rate
    ) |>
  full_join(filter(viol_crime_by_off_state, year == 2019, crime == "Homicide"), by = "state") |>
  transmute(state, state_abb, hom_21, hom_rate_21, hom_clear_21,
            hom_19 = actual, hom_rate_19 = actual / pop_total, hom_clear_19 = clearance_rate)

to_map_hom <- hom_change |>
  mutate(
    change = hom_rate_21 - hom_rate_19,
    pct_change = change / hom_rate_19,
    status = case_when(
      pct_change > 0 ~ 1,
      pct_change <= 0 ~ 2,
      is.na(pct_change) ~ 3,
    ),
    url = paste0("https://projects.csgjusticecenter.org/tools-for-states-to-address-crime/50-state-crime-data/?state=", tolower(state_abb))
  )

to_map_hom_clear <- hom_change |>
  mutate(
    change = hom_clear_21 - hom_clear_19,
    pct_change = change / hom_clear_19,
    status = case_when(
      pct_change > 0 ~ 1,
      pct_change <= 0 ~ 2,
      is.na(pct_change) ~ 3,
    ),
    url = paste0("https://projects.csgjusticecenter.org/tools-for-states-to-address-crime/50-state-crime-data/?state=", tolower(state_abb))
  )

viol_crime_hex <- make_hex_map(to_map_viol_crime, map_colors)
hom_hex <- make_hex_map(to_map_hom, map_colors)

viol_crime_clear_hex <- make_hex_map(to_map_viol_crime_clear, map_colors_rev)
hom_clear_hex <- make_hex_map(to_map_hom_clear, map_colors_rev)


saveWidget(viol_crime_hex, "viol-crime-hex-map.html", selfcontained = FALSE, libdir = "map-libs")
saveWidget(hom_hex, "hom-hex-map.html", selfcontained = FALSE, libdir = "map-libs")

saveWidget(viol_crime_clear_hex, "viol-crime-clear-hex-map.html", selfcontained = FALSE, libdir = "map-libs")
saveWidget(hom_clear_hex, "hom-clear-hex-map.html", selfcontained = FALSE, libdir = "map-libs")
