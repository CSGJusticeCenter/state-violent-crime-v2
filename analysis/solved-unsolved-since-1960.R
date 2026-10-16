## Solved and unsolved violent crime in the US, 1965-2025
##
## Reported counts come from Crime Index state tables before 1979 and the FBI
## state estimates from 1979. Crime Index digitized its tables from the FBI's
## Crime in the United States.
##
## Solved counts:
## - 2015-2025 use the site's published method, us_solve_trend()
## - 1965-2014 rebuild that method from agency counts: one pooled clearance rate
##   from state totals, applied to US reported crime. The pool leaves out
##   state-years where agencies with at least min_screen_reported violent
##   offenses and no clearances hold at least max_no_clearance_share of reported
##   crime, and Illinois after illinois_last_year
##
## A year's clearances can include crimes from earlier years.

library(tidyverse)
library(csgjcr)
library(scales)
source("R/page-data.R")

first_year <- 1965
fbi_count_year <- 1979
published_year <- 2015

### Illinois clearances are plausible through this year
illinois_last_year <- 1985

raw_path <- function(...) csg_sp_path("jr_data_library", "data", "raw", ...)
crimeindex_path <- function(file) raw_path("ahd", "crime_index", file)

srs <- read_srs_state()

### Agency clearances ---------------------------------------------------------

### Return A agency counts before 2000
kaplan <- read_rds(raw_path("fbi", "srs", "kaplan", "offenses_known_yearly_1960_2024.rds")) |>
  filter(between(year, first_year, 1999)) |>
  transmute(
    year,
    ori,
    state_abbr = as.character(as.vector(state_abb)),
    reported = as.vector(actual_index_violent),
    cleared = coalesce(as.vector(total_cleared_index_violent), 0)
  ) |>
  as_tibble()

### CDE agency counts from 2000, with NYPD's 2022 clearances repaired
nypd_clearance <- read_csv("data/nypd_clearance_2022.csv", show_col_types = FALSE)

cde <- read_rds(csg_sp_path("jr_data_library", "data", "analysis", "fbi", "srs", "fbi_srs_agency.rds")) |>
  filter(group %in% violent_offenses, indicator %in% c("Incidents reported", "Incidents cleared")) |>
  mutate(indicator = if_else(indicator == "Incidents reported", "reported", "cleared")) |>
  summarize(n = sum(n, na.rm = TRUE), .by = c(year, ori, state_abbr, group, indicator)) |>
  pivot_wider(names_from = indicator, values_from = n, values_fill = 0) |>
  left_join(nypd_clearance, by = join_by(year, ori, group), relationship = "one-to-one") |>
  mutate(cleared = if_else(is.na(clearance_rate), cleared, reported * clearance_rate)) |>
  summarize(across(c(reported, cleared), sum), .by = c(year, ori, state_abbr)) |>
  mutate(state_abbr = as.character(state_abbr))

agencies <- bind_rows(kaplan, cde) |>
  filter(reported > 0) |>
  mutate(non_reporting = reported >= min_screen_reported & cleared == 0)

### State reported counts -----------------------------------------------------

fbi_reported <- srs |>
  filter(crime_cat == "Violent", year >= fbi_count_year) |>
  summarize(incidents_reported = sum(incidents_reported), .by = c(year, state_abbr)) |>
  mutate(state_abbr = as.character(state_abbr))

crimeindex_crime <- read_csv(
  crimeindex_path("historical_crime_1930_2025_1790092670.csv"),
  col_types = cols(id = "c", .default = "d")
)

### Crime Index has no DC row, so DC is the US total less the 50 states
ci_reported <- crimeindex_crime |>
  filter(str_detect(id, "^[a-z]{2}$"), between(year, first_year, fbi_count_year - 1)) |>
  transmute(year, state_abbr = str_to_upper(id), incidents_reported = violent_total)

ci_reported <- ci_reported |>
  summarize(
    incidents_reported = 2 * sum(incidents_reported[state_abbr == "US"]) - sum(incidents_reported),
    .by = year
  ) |>
  mutate(state_abbr = "DC") |>
  bind_rows(filter(ci_reported, state_abbr != "US"))

state_reported <- bind_rows(ci_reported, fbi_reported)

if (any(count(state_reported, year)$n != 51)) stop("Every year needs reported counts for 50 states and DC")

### Site method from agency counts --------------------------------------------

site_replica_rates <- agencies |>
  filter(state_abbr %in% state_reported$state_abbr) |>
  summarize(
    share_no_clearances = sum(reported[non_reporting]) / sum(reported),
    across(c(reported, cleared), sum),
    .by = c(year, state_abbr)
  ) |>
  filter(
    share_no_clearances < max_no_clearance_share,
    !(state_abbr == "IL" & year > illinois_last_year)
  ) |>
  summarize(pct_site_replica = sum(cleared) / sum(reported), .by = year)

site_replica_us <- state_reported |>
  summarize(incidents_reported = sum(incidents_reported), .by = year) |>
  left_join(site_replica_rates, by = join_by(year), relationship = "one-to-one") |>
  mutate(incidents_solved = incidents_reported * pct_site_replica) |>
  select(-pct_site_replica)

### Agency-based state rates --------------------------------------------------

### a check: each state's rate from agencies that report clearances, applied to
### its own reported count. Illinois after illinois_last_year, and states with
### no clearances left, take the national rate

state_rates <- agencies |>
  filter(!non_reporting, state_abbr %in% state_reported$state_abbr) |>
  summarize(across(c(reported, cleared), sum), .by = c(year, state_abbr)) |>
  mutate(
    state_rate = if_else(cleared > 0, cleared / reported, NA),
    state_rate = if_else(state_abbr == "IL" & year > illinois_last_year, NA, state_rate)
  )

national_rates <- state_rates |>
  filter(!is.na(state_rate)) |>
  summarize(national_rate = sum(cleared) / sum(reported), .by = year)

agency_states <- state_reported |>
  left_join(select(state_rates, year, state_abbr, state_rate), by = join_by(year, state_abbr), relationship = "one-to-one") |>
  left_join(national_rates, by = join_by(year), relationship = "many-to-one") |>
  mutate(
    national_fallback = is.na(state_rate),
    incidents_solved = incidents_reported * coalesce(state_rate, national_rate)
  )

agency_us <- agency_states |>
  summarize(across(c(incidents_reported, incidents_solved), sum), .by = year)

### US population -------------------------------------------------------------

### the FBI state estimates carry population from 2000; homicide rows carry each
### state's population once
srs_population <- srs |>
  filter(group == "Homicide", !is.na(pop_total)) |>
  summarize(population = sum(pop_total), .by = year)

us_population <- crimeindex_crime |>
  filter(id == "us", year >= first_year, !year %in% srs_population$year) |>
  select(year, population) |>
  bind_rows(srs_population)

### Spliced series ------------------------------------------------------------

published <- us_solve_trend(srs, published_year) |>
  select(year, incidents_reported, incidents_solved)

trend <- bind_rows(
  `site method from agency counts` = filter(site_replica_us, year < published_year),
  `published site method` = published,
  .id = "solve_method"
) |>
  left_join(us_population, by = join_by(year), relationship = "one-to-one") |>
  mutate(
    count_source = if_else(year < fbi_count_year, "Crime Index", "FBI state estimates"),
    incidents_unsolved = incidents_reported - incidents_solved,
    pct_solved = incidents_solved / incidents_reported,
    across(
      c(incidents_reported, incidents_solved, incidents_unsolved),
      \(x) x / population * 1e5,
      .names = "{str_remove(.col, 'incidents_')}_per_100k"
    )
  ) |>
  select(year, count_source, solve_method, population, starts_with("incidents"), pct_solved, ends_with("per_100k")) |>
  arrange(year)

if (anyNA(trend$population)) stop("The spliced series has years without population")

if (!all(diff(trend$year) == 1)) stop("The spliced series skips or repeats a year")

write_csv(trend, "analysis/solved-unsolved-since-1960.csv")

### Checks --------------------------------------------------------------------

### state-years that take the national rate, outside Illinois
agency_states |>
  filter(national_fallback, state_abbr != "IL", year < published_year) |>
  summarize(years = str_flatten(year, " "), .by = state_abbr) |>
  print()

### Crime Index's FBI-table clearance rates, which leave out agencies without clearances
crimeindex_offenses <- c(murder = "Homicide", rape = "Rape", robbery = "Robbery", assault = "Aggravated assault")
ci_counts <- crimeindex_crime |>
  filter(id == "us") |>
  select(year, str_c(names(crimeindex_offenses), "_total")) |>
  pivot_longer(-year, names_to = "offense", names_pattern = "(.*)_total", values_to = "reported")
ci_solve <- read_csv(crimeindex_path("historical_clearance_1960_2025_1790092670.csv"), col_types = cols(id = "c", .default = "d")) |>
  filter(id == "us") |>
  select(year, str_c(names(crimeindex_offenses), "_cleared_rate")) |>
  pivot_longer(-year, names_to = "offense", names_pattern = "(.*)_cleared_rate", values_to = "pct_cleared") |>
  inner_join(ci_counts, by = join_by(year, offense), relationship = "one-to-one") |>
  summarize(pct_crimeindex = sum(reported * pct_cleared / 100) / sum(reported), .by = year)

### agency rates, the site method on FBI-estimated clearances from their first
### year, the site method rebuilt from agency counts, and Crime Index
site_method <- us_solve_trend(srs, 1985) |>
  transmute(year, pct_site_method = incidents_solved / incidents_reported)

agency_us |>
  transmute(year, pct_agency = incidents_solved / incidents_reported) |>
  left_join(site_method, by = join_by(year)) |>
  left_join(site_replica_rates, by = join_by(year)) |>
  left_join(ci_solve, by = join_by(year)) |>
  mutate(across(starts_with("pct"), \(x) round(100 * x, 1))) |>
  print(n = Inf)

### Charts --------------------------------------------------------------------

chart_colors <- c(reported = "#0B1F35", solved = "#0A8A6A", gap = "#B9CBD8", ink = "#0B1F35", muted = "#5B6B7C", surface = "#EEF3F9")
chart_font <- "Public Sans"

chart_sources <- str_c(
  "Sources: FBI Crime Data Explorer; FBI Uniform Crime Reports via Jacob Kaplan; ",
  "Crime Index (AH Datalytics) historical tables, 1965–1978"
)

short_count <- function(x) if_else(x >= 1e6, sprintf("%.2fM", x / 1e6), sprintf("%.0fK", x / 1e3))

### reported and solved lines with the unsolved gap shaded
### df has year, reported and solved; label_value formats the labeled values
solved_chart <- function(df, label_value, y_breaks, y_labels, title, note) {
  y_max <- max(y_breaks) * 1.1

  ### first-year values sit left of their points, peak-year values above
  ### reported and below solved, last-year values beside the series names
  first <- slice_min(df, year, n = 1)
  peak <- slice_max(df, reported, n = 1)
  last <- slice_max(df, year, n = 1)

  value_labels <- tibble(
    year = c(first$year, first$year, peak$year, peak$year),
    value = c(first$reported, first$solved, peak$reported, peak$solved),
    series = c("reported", "solved", "reported", "solved"),
    x = c(first$year - 1.2, first$year - 1.2, peak$year, peak$year),
    hjust = c(1, 1, 0.5, 0.5),
    vjust = c(0.5, 0.5, -0.9, 1.9)
  ) |>
    mutate(label = label_value(value))

  points <- bind_rows(
    select(value_labels, year, value, series),
    tibble(year = last$year, value = c(last$reported, last$solved), series = c("reported", "solved"))
  )

  series_labels <- tibble(
    y = c(last$reported, (last$reported + last$solved) / 2, last$solved),
    label = c(
      str_c("Reported\n", label_value(last$reported)),
      "Unsolved\n(shaded gap)",
      str_c("Solved\n", label_value(last$solved))
    ),
    color = chart_colors[c("reported", "muted", "solved")],
    size = c(4.4, 3.8, 4.4)
  )

  ggplot(df, aes(year)) +
    geom_ribbon(aes(ymin = solved, ymax = reported), fill = chart_colors[["gap"]]) +
    geom_line(aes(y = reported), color = chart_colors[["reported"]], linewidth = 1) +
    geom_line(aes(y = solved), color = chart_colors[["solved"]], linewidth = 1) +
    geom_point(
      data = points,
      aes(y = value, color = series),
      size = 2.6, stroke = 1.2, shape = 21, fill = chart_colors[["surface"]]
    ) +
    geom_text(
      data = value_labels,
      aes(x = x, y = value, label = label, color = series, hjust = hjust, vjust = vjust),
      family = chart_font, fontface = "bold", size = 3.6
    ) +
    geom_text(
      data = series_labels,
      aes(x = last$year + 1.5, y = y, label = label),
      color = series_labels$color, size = series_labels$size,
      hjust = 0, lineheight = 0.9, family = chart_font, fontface = "bold"
    ) +
    scale_color_manual(values = chart_colors[c("reported", "solved")], guide = "none") +
    scale_x_continuous(breaks = seq(first_year, 2025, 10), expand = expansion(add = c(6, 13))) +
    scale_y_continuous(limits = c(0, y_max), breaks = y_breaks, labels = y_labels, expand = expansion(mult = 0)) +
    labs(title = title, x = NULL, y = NULL, caption = str_c(note, "\n", chart_sources)) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_family = chart_font, base_size = 12) +
    theme(
      plot.background = element_rect(fill = chart_colors[["surface"]], color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#D3DCE6", linewidth = 0.4),
      axis.line.x = element_line(color = chart_colors[["ink"]], linewidth = 0.8),
      axis.text.x = element_text(color = chart_colors[["ink"]], face = "bold", size = 11),
      axis.text.y = element_text(color = chart_colors[["muted"]], size = 11),
      plot.title = element_text(color = chart_colors[["ink"]], face = "bold", size = 18, margin = margin(b = 16)),
      plot.title.position = "plot",
      plot.caption = element_text(color = chart_colors[["muted"]], size = 8.5, hjust = 0, lineheight = 1.1, margin = margin(t = 14)),
      plot.caption.position = "plot",
      plot.margin = margin(20, 20, 16, 20)
    )
}

save_chart <- function(chart, file) {
  ggsave(file, chart, width = 9, height = 6, dpi = 200, device = ragg::agg_png, bg = chart_colors[["surface"]])
}

trend |>
  select(year, reported = incidents_reported, solved = incidents_solved) |>
  solved_chart(
    label_value = short_count,
    y_breaks = seq(0, 2e6, 5e5),
    y_labels = label_number(scale_cut = cut_short_scale(), drop0trailing = TRUE),
    title = "Violent crime reported and solved in the US, 1965–2025",
    note = "Solved counts apply clearance rates to estimated violent crimes. Clearances can include crimes from earlier years."
  ) |>
  save_chart("analysis/solved-unsolved-since-1960.png")

trend |>
  select(year, reported = reported_per_100k, solved = solved_per_100k) |>
  solved_chart(
    label_value = \(x) comma(x, 1),
    y_breaks = seq(0, 800, 200),
    y_labels = comma,
    title = "Violent crime reported and solved per 100,000 residents",
    note = "US rates per 100,000 residents, 1965–2025. Solved rates apply clearance rates to estimated violent crimes. Clearances can include crimes from earlier years."
  ) |>
  save_chart("analysis/solved-unsolved-per-capita-since-1960.png")
