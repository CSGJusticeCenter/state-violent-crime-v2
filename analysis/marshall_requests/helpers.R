### shared setup, data readers and chart helpers for
### analysis/marshall_requests/marshall-ncsl-main-presentation.qmd
### sourced from the project root, after R/page-data.R and R/cde-api.R

### where this request keeps its inputs and outputs
marshall_dir <- "analysis/marshall_requests"
marshall_data_dir <- file.path(marshall_dir, "data")
marshall_png_dir <- file.path(marshall_dir, "png")

### site look: gt america, transparent background, site_colors from R/page-data.R
chart_font <- "GT America"
if (!chart_font %in% systemfonts::system_fonts()$family) stop(chart_font, " is not installed")

### the 20-year window most slides use
first_year <- 2005

### paths -----------------------------------------------------------------------

### data snapshot project and 50 states, 1 goal files sit outside the data library
snapshot_path <- function(...) {
  file.path(csgjcr::csg_sp_path(), "Data_Action_Justice_Data_Snapshots", ...)
}

recidivism_path <- function() {
  file.path(
    csgjcr::csg_sp_path(), "z_ARCHIVE", "50 State Revocations Project",
    "State Recidivism Compendium", "data for analysis", "2026_50_state_1_goal_long_update.csv"
  )
}

### a drop-in csv someone adds by hand; NULL when it isn't there yet, and a loud
### stop when its columns don't match what the slide needs
read_drop_in <- function(file, cols) {
  path <- file.path(marshall_data_dir, file)
  if (!file.exists(path)) return(NULL)
  df <- read_csv(path, show_col_types = FALSE)
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(file, " needs these columns: ", paste(cols, collapse = ", "),
         " (missing ", paste(missing, collapse = ", "), ")", call. = FALSE)
  }
  df
}

### chart look ------------------------------------------------------------------

### same theme as R/05-export-backlog-pngs.R, pulled into one function
theme_slide <- function(base_size = 10) {
  theme_minimal(base_family = chart_font, base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = site_colors[["grid"]], linewidth = 0.4),
      axis.line.x = element_line(color = site_colors[["baseline"]], linewidth = 0.4),
      axis.ticks.x = element_line(color = site_colors[["baseline"]], linewidth = 0.4),
      axis.text = element_text(color = site_colors[["muted"]], size = 10),
      plot.title = element_text(color = site_colors[["text"]], face = "bold", size = 16.5, margin = margin(b = 6)),
      plot.subtitle = element_text(color = site_colors[["muted"]], size = 11, margin = margin(b = 12)),
      plot.title.position = "plot",
      plot.caption = element_text(color = site_colors[["muted"]], size = 9, hjust = 0, lineheight = 1.15,
                                  margin = margin(t = 12)),
      plot.caption.position = "plot",
      strip.text = element_text(color = site_colors[["text"]], face = "bold", size = 11.5, hjust = 0),
      legend.position = "none",
      ### room on the right for line names
      plot.margin = margin(14, 90, 10, 14)
    )
}

### labels like 1.31M, 563K, 25.3K or 840, same as the site
compact_label <- function(x) {
  case_when(
    abs(x) >= 1e6 ~ sprintf("%.2fM", x / 1e6),
    abs(x) >= 1e5 ~ sprintf("%.0fK", x / 1e3),
    abs(x) >= 1e3 ~ sprintf("%.1fK", x / 1e3),
    .default = sprintf("%.0f", x)
  )
}

### big counts short (1.31M, 563K), rates and small counts in full (3,911)
count_or_rate <- function(x) if_else(abs(x) >= 1e5, compact_label(x), comma(round(x)))

pct_change <- function(from, to) 100 * (to - from) / from

### "-48%" or "+12%"
pct_label <- function(x) paste0(if_else(x > 0, "+", ""), round(x), "%")

### text geoms need the chart font spelled out each time
slide_text <- function(...) geom_text(family = chart_font, ...)

### chart caption from plain sentences: notes wrap to the chart width, and the
### source always gets its own line, so the qmd can list sentences one per line
slide_caption <- function(..., source, width = 170) {
  notes <- paste(c(...), collapse = " ")
  lines <- c(if (nzchar(notes)) str_wrap(notes, width), str_wrap(paste("Source:", source), width))
  paste(lines, collapse = "\n")
}

### line + points + a bold name at the right end, the site's way of skipping legends
### df needs year, value and series; colors is a named vector by series
labeled_lines <- function(df, colors, label_df = NULL, digits_fn = compact_label, linetypes = NULL,
                          nudge_x = 0.5) {
  ends <- df |>
    filter(!is.na(value)) |>
    group_by(series) |>
    slice_max(year, n = 1) |>
    ungroup()
  list(
    geom_line(aes(year, value, color = series, linetype = series), linewidth = 1.1, na.rm = TRUE),
    geom_point(aes(year, value, fill = series), shape = 21, size = 2.2, stroke = 0.7, color = "white", na.rm = TRUE),
    geom_text(data = ends, aes(year, value, label = series, color = series), hjust = 0, nudge_x = nudge_x,
              family = chart_font, fontface = "bold", size = 3.9),
    scale_color_manual(values = colors),
    scale_fill_manual(values = colors),
    scale_linetype_manual(values = linetypes %||% setNames(rep("solid", length(colors)), names(colors)))
  )
}

### series names with each line's change from its first to last year appended
### ("Parole violations: -25%"), named by the original series, for end labels
change_names <- function(df) {
  ends <- df |>
    filter(!is.na(value)) |>
    group_by(series) |>
    summarise(change = pct_change(value[which.min(year)], value[which.max(year)]))
  setNames(paste0(ends$series, ": ", pct_label(ends$change)), ends$series)
}

### value labels on the first and last points of each series
end_value_labels <- function(df, label_fn = compact_label, size = 3.4) {
  ends <- df |>
    filter(!is.na(value)) |>
    group_by(series) |>
    filter(year %in% range(year)) |>
    ungroup()
  geom_text(data = ends, aes(year, value, label = label_fn(value), color = series),
            vjust = -1.1, family = chart_font, size = size)
}

### small multiples of single trend lines, one panel per measure, each on its own
### y axis from zero, with the first and last values labeled. df needs year,
### panel and value; colors is a named vector by panel. an NA value (like 2021)
### leaves a gap in the line
### from_zero is TRUE (every panel starts at zero), FALSE (each panel's y axis
### fits its own range, to show change) or the names of the panels that start at zero
trend_panels <- function(df, colors, label_fn = count_or_rate, ncol = NULL, years = range(df$year),
                         from_zero = TRUE) {
  zero_panels <- if (isTRUE(from_zero)) names(colors) else if (isFALSE(from_zero)) character() else from_zero
  df <- mutate(df, panel = factor(panel, levels = names(colors)))
  points <- filter(df, !is.na(value))
  ### invisible points set each panel's y range: zero, or the line's full range
  ### below its lowest value (never below zero), so change shows without exaggerating it
  y_floor <- points |>
    group_by(panel) |>
    summarise(value = if_else(first(panel) %in% zero_panels, 0,
                              max(0, min(value) - 1 * (max(value) - min(value)))),
              year = years[1], .groups = "drop")
  ### first value sits above its point, last value to the right of its point,
  ### so neither lands on the line. on a tight axis, a first value goes below
  ### its point when the line rises from it
  firsts <- points |>
    group_by(panel) |>
    arrange(year, .by_group = TRUE) |>
    mutate(below = !panel %in% zero_panels & lead(value) > value,
           vjust = if_else(below, 2, -1.2),
           ### shifted right when below, clear of the y axis labels
           hjust = if_else(below, 0.2, 0.5)) |>
    slice_min(year, n = 1) |>
    ungroup()
  lasts <- points |> group_by(panel) |> slice_max(year, n = 1) |> ungroup()
  ggplot(df, aes(year, value, color = panel)) +
    geom_line(linewidth = 1.1, na.rm = TRUE) +
    geom_point(aes(fill = panel), shape = 21, size = 2, stroke = 0.6, color = "white", na.rm = TRUE) +
    geom_blank(data = y_floor) +
    geom_text(data = firsts, aes(label = label_fn(value), vjust = vjust, hjust = hjust), family = chart_font, size = 3.4,
              fontface = "bold") +
    geom_text(data = lasts, aes(label = label_fn(value)), hjust = 0, nudge_x = 0.5, family = chart_font,
              size = 3.4, fontface = "bold") +
    facet_wrap(~panel, scales = "free_y", ncol = ncol) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_x_continuous(breaks = seq(5 * ceiling(years[1] / 5), years[2], 5),
                       expand = expansion(add = c(1, 0.12 * diff(years) + 1))) +
    scale_y_continuous(labels = label_fn, expand = expansion(mult = c(0, 0.3))) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) +
    theme_slide() +
    theme(plot.margin = margin(14, 24, 10, 14), panel.spacing.x = unit(2.2, "lines"))
}

### save the pngs for the deck, sized for a 16:9 slide: one with the caption and
### one without (name-no-caption.png), for slides that put the notes elsewhere
save_slide <- function(plot, name, width = 10, height = 5.6) {
  dir.create(marshall_png_dir, showWarnings = FALSE)
  save_png <- function(p, file) {
    ggsave(file.path(marshall_png_dir, file), p,
           width = width, height = height, dpi = 300, bg = "transparent", device = ragg::agg_png)
  }
  save_png(plot, paste0(name, ".png"))
  save_png(plot + labs(caption = NULL), paste0(name, "-no-caption.png"))
  invisible(plot)
}

### population ------------------------------------------------------------------

### u.s. population by year: total and adults
read_us_population <- function() {
  read_rds(jr_data_path("analysis", "census", "pep", "census_pep_state.rds")) |>
    filter(group == "Total") |>
    group_by(year) |>
    summarise(pop_total = sum(pop_total), pop_adult = sum(pop_adult), .groups = "drop")
}

### non-hispanic white and black residents (race alone), u.s., from acs 5-year
### table b03002, one row per ending year, as in the data snapshot. cached in
### data/, since the census api is only needed when a new vintage comes out.
### a pull needs CENSUS_API_KEY in .Renviron
acs_cache_path <- file.path(marshall_data_dir, "acs-race-national.csv")

read_acs_race <- function(years = 2009:2024, refresh = FALSE) {
  if (file.exists(acs_cache_path) && !refresh) {
    cached <- read_csv(acs_cache_path, show_col_types = FALSE)
    if (all(years %in% cached$acs_year)) return(filter(cached, acs_year %in% years))
  }
  key <- Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(key)) {
    stop("data/acs-race-national.csv is missing years, and pulling them needs CENSUS_API_KEY in .Renviron ",
         "(free key: https://api.census.gov/data/key_signup.html)", call. = FALSE)
  }
  rows <- map(years, \(y) {
    url <- sprintf("https://api.census.gov/data/%d/acs/acs5?get=B03002_003E,B03002_004E&for=us:1&key=%s", y, key)
    x <- jsonlite::fromJSON(url)
    tibble(acs_year = y, pop_white_nh = as.numeric(x[2, 1]), pop_black_nh = as.numeric(x[2, 2]))
  }) |>
    list_rbind()
  dir.create(marshall_data_dir, showWarnings = FALSE)
  write_csv(rows, acs_cache_path)
  rows
}

### slide 3: crime ---------------------------------------------------------------

violent_offenses <- c("Homicide", "Rape", "Robbery", "Aggravated assault")
property_offenses <- c("Burglary", "Larceny", "Motor vehicle theft")

### u.s. crime rates per 100k, adding up the 50 states and dc from the fbi's
### state estimates
us_crime_rates <- function(from = first_year) {
  srs <- read_rds(jr_data_path("analysis", "fbi", "srs", "fbi_srs_estimated_crimes_state.rds")) |>
    filter(indicator == "Incidents reported", year >= from)
  pop <- srs |>
    distinct(year, state_abbr, pop_total) |>
    group_by(year) |>
    summarise(pop = sum(pop_total), .groups = "drop")
  srs |>
    mutate(type = if_else(group %in% violent_offenses, "Violent crime", "Property crime")) |>
    group_by(year, type) |>
    summarise(n = sum(n), .groups = "drop") |>
    pivot_wider(names_from = type, values_from = n) |>
    mutate(`All crime` = `Violent crime` + `Property crime`) |>
    left_join(pop, by = "year") |>
    pivot_longer(c(`All crime`, `Violent crime`, `Property crime`), names_to = "series", values_to = "n") |>
    mutate(value = 1e5 * n / pop)
}

### slides 4, 5, 7: cde arrests --------------------------------------------------

### cde arrest offense ids; rape arrests sit under the legacy definition
cde_violent_ids <- c(Homicide = 11, Rape = 20, Robbery = 30, `Aggravated assault` = 50)

### ages the fbi counts as juvenile (under 18)
juvenile_ages <- c("Under 10", "Under 11", "10-12", "11-12", "13-14", "15", "16", "17", "Juvenile Other")

### every arrest number the slides use, one tidy row per year x offense x field
### x category; pulled once from the cde api and cached, since a full pull is
### about 250 calls
cde_cache_path <- file.path(marshall_data_dir, "cde-arrests-national.csv")

read_cde_arrests <- function(years, offenses = c("all", cde_violent_ids), refresh = FALSE) {
  if (file.exists(cde_cache_path) && !refresh) {
    cached <- read_csv(cde_cache_path, show_col_types = FALSE, col_types = cols(offense = "c"))
    have <- distinct(cached, year, offense)
    want <- expand_grid(year = years, offense = as.character(offenses))
    if (nrow(anti_join(want, have, by = c("year", "offense"))) == 0) {
      return(semi_join(cached, want, by = c("year", "offense")))
    }
  }
  rows <- expand_grid(year = years, offense = as.character(offenses)) |>
    pmap(\(year, offense) cde_arrest_rows(offense, year)) |>
    list_rbind()
  dir.create(marshall_data_dir, showWarnings = FALSE)
  write_csv(rows, cde_cache_path)
  rows
}

### one offense-year from the api, flattened; "all" also brings the share of
### the population covered by reporting agencies
cde_arrest_rows <- function(offense, year) {
  totals <- cde_arrest_call(offense, year, "totals")
  flatten <- function(field) {
    x <- unlist(totals[[field]])
    if (length(x) == 0) return(NULL)
    tibble(field = field, category = names(x), value = as.numeric(x))
  }
  out <- bind_rows(
    flatten("Arrestee Sex"), flatten("Arrestee Race"), flatten("Offense Name"),
    flatten("Male Arrests By Age"), flatten("Female Arrests By Age")
  )
  if (offense == "all") {
    counts <- cde_arrest_call(offense, year, "counts")
    coverage <- mean(unlist(counts$tooltips[["Percent of Population Coverage"]][[1]])) / 100
    out <- bind_rows(out, tibble(field = "Coverage", category = "Share of population covered", value = coverage))
  }
  mutate(out, year = year, offense = offense, .before = 1)
}

### same key handling, retries and key masking as read_cde_summarized()
cde_arrest_call <- function(offense, year, type, tries = 6) {
  url <- sprintf(
    "https://api.usa.gov/crime/fbi/cde/arrest/national/%s?type=%s&from=01-%d&to=12-%d&API_KEY=%s",
    offense, type, year, year, cde_api_key()
  )
  for (attempt in seq_len(tries)) {
    resp <- tryCatch(curl::curl_fetch_memory(url), error = function(e) e)
    status <- if (inherits(resp, "error")) NA else resp$status_code
    if (!is.na(status) && status == 200) return(jsonlite::fromJSON(rawToChar(resp$content)))
    if (!is.na(status) && status >= 400 && status < 500 && status != 429) break
    if (attempt < tries) Sys.sleep(2^attempt)
  }
  problem <- if (is.na(status)) conditionMessage(resp) else paste("HTTP status", status)
  stop("Could not read CDE arrests ", offense, " ", type, " for ", year, ": ",
       gsub(cde_api_key(), "<key>", problem, fixed = TRUE), call. = FALSE)
}

### the fbi's official national arrest estimates (cius table 29, table 18 in
### 2016), typed in from each year's release. 2021 is blank: the fbi published no
### national estimate that year
read_fbi_arrest_estimates <- function() {
  read_csv(jr_data_path("raw", "fbi", "cius", "fbi_drug_arrests_2005_2025.csv"), show_col_types = FALSE) |>
    filter(!is.na(total_arrests_est)) |>
    transmute(year, `All arrests` = total_arrests_est, `Drug arrests` = drug_abuse_violation_arrests_est)
}

### black-to-white relative rate index: (black arrests / non-hispanic black
### residents) over (white arrests / non-hispanic white residents), with the acs
### 5-year estimates ending in each year, as in the data snapshot. years before
### the first 5-year release use the first one, and years past the latest use the
### latest. coverage hits both groups alike, so it uses reported counts as they are
arrest_rate_ratio <- function(arrests, acs) {
  arrests |>
    filter(offense == "all", field == "Arrestee Race", category %in% c("Black or African American", "White")) |>
    select(year, category, value) |>
    pivot_wider(names_from = category, values_from = value) |>
    mutate(acs_year = pmin(pmax(year, min(acs$acs_year)), max(acs$acs_year))) |>
    left_join(acs, by = "acs_year") |>
    mutate(
      black_rate = 1e5 * `Black or African American` / pop_black_nh,
      white_rate = 1e5 * White / pop_white_nh,
      value = black_rate / white_rate
    )
}

### u.s. residents ages 10-17 by year, from the census single-year-of-age files.
### each vintage covers its own decade, so years don't overlap
read_us_youth_population <- function() {
  files <- c("sc-est2009-alldata6-all.csv", "sc-est2019-alldata6.csv", "sc-est2025-alldata6.csv")
  map(files, \(f) {
    read_csv(jr_data_path("raw", "census", "pep", f), show_col_types = FALSE,
             col_select = c(SEX, ORIGIN, AGE, starts_with("POPESTIMATE"))) |>
      filter(SEX == 0, ORIGIN == 0, AGE %in% 10:17) |>
      summarise(across(starts_with("POPESTIMATE"), sum)) |>
      pivot_longer(everything(), names_to = "year", values_to = "pop_youth",
                   names_transform = \(x) as.numeric(sub("POPESTIMATE", "", x)))
  }) |>
    list_rbind() |>
    distinct(year, .keep_all = TRUE)
}

youth_series <- "Youth (ages 10–17)"
adult_series <- "Adults (18 and older)"

### violent arrests (homicide, rape, robbery, aggravated assault) for youth and
### adults, scaled by coverage, as rates per 100k residents ages 10-17 and 18+.
### youth arrests include the few under 10, as in the fbi and ojjdp rates.
### years without population have an NA rate but keep their estimated count
youth_adult_violent <- function(arrests, pop, youth_pop) {
  coverage <- arrests |>
    filter(offense == "all", field == "Coverage") |>
    select(year, coverage = value)
  arrests |>
    filter(offense %in% as.character(cde_violent_ids), field %in% c("Male Arrests By Age", "Female Arrests By Age")) |>
    mutate(series = if_else(category %in% juvenile_ages, youth_series, adult_series)) |>
    group_by(year, series) |>
    summarise(reported = sum(value), .groups = "drop") |>
    left_join(coverage, by = "year") |>
    left_join(select(pop, year, pop_adult), by = "year") |>
    left_join(youth_pop, by = "year") |>
    mutate(estimated = reported / coverage,
           population = if_else(series == youth_series, pop_youth, pop_adult),
           value = 1e5 * estimated / population) |>
    select(year, series, reported, estimated, population, value)
}

### slides 6, 8, 22: prisons ------------------------------------------------------

### national prisoner statistics, 1978-2022, one row per state and year; the
### "ST" row is the 50-state total. negative codes mean missing or not
### applicable
read_nps <- function() {
  read_tsv(
    jr_data_path("raw", "bjs", "nps", "ICPSR_38871-V1", "ICPSR_38871", "DS0001", "38871-0001-Data.tsv"),
    show_col_types = FALSE,
    col_select = c(YEAR, STATE, JURTOTM, JURTOTF, COMMITM, COMMITF, PARNOM, PARNOF, PARNEWM, PARNEWF,
                   ADCRNOM, ADCRNOF, ADCRNEWM, ADCRNEWF)
  ) |>
    mutate(across(-c(YEAR, STATE), \(x) if_else(x < 0, NA_real_, x))) |>
    transmute(
      year = YEAR, state_abbr = STATE,
      prison_pop = JURTOTM + JURTOTF,
      new_court = COMMITM + COMMITF,
      ### supervision violations: anyone returned from parole or other conditional
      ### release, with or without a new sentence
      violation_technical = PARNOM + PARNOF + coalesce(ADCRNOM, 0) + coalesce(ADCRNOF, 0),
      violation_new_sentence = PARNEWM + PARNEWF + coalesce(ADCRNEWM, 0) + coalesce(ADCRNEWF, 0),
      violations = violation_technical + violation_new_sentence
    )
}

### prison admissions for probation and parole violations, by state, 2018-2023,
### from csg's more community, less confinement survey (in z_ARCHIVE). keeps the
### states that report both every year, so changes in who reports don't drive the trend
read_mclc_violations <- function() {
  agg <- read_rds(file.path(
    csgjcr::csg_sp_path(), "z_ARCHIVE", "50 State Revocations Project", "MCLC Shiny App",
    "data", "analysis", "svii_agg.rds"
  ))
  agg |>
    filter(type == "Admissions", violation_type == "Both", supervision_type %in% c("Probation", "Parole")) |>
    transmute(state_abbr = as.character(state_abbr), year,
              series = paste(supervision_type, "violations"), value = n) |>
    group_by(state_abbr) |>
    filter(sum(!is.na(value)) == n_distinct(agg$year) * 2) |>
    ungroup()
}

### 2023 prison counts under state jurisdiction from the prisoners series, which
### runs a year past the nps file
read_prisoners_2023 <- function() {
  read_csv(jr_data_path("analysis", "bjs", "prisoners_series", "prisoners_by_jurisdiction.csv"),
           show_col_types = FALSE) |>
    filter(year == 2023, geo_name %in% c(state.name, "District of Columbia")) |>
    mutate(state_abbr = state.abb[match(geo_name, state.name)]) |>
    select(year, geo_name, state_abbr, prison_pop = total)
}

### state prison population and rate per 100k residents, matching the data
### snapshot (everyone under state jurisdiction, all residents)
us_prison_rate <- function(nps, prisoners_2023, pop, from = first_year) {
  nps |>
    filter(state_abbr == "ST", year >= from) |>
    select(year, prison_pop) |>
    bind_rows(summarise(prisoners_2023, year = 2023, prison_pop = sum(prison_pop))) |>
    left_join(pop, by = "year") |>
    mutate(value = 1e5 * prison_pop / pop_total)
}

### slide 9: recidivism -------------------------------------------------------------

read_recidivism <- function() {
  read_csv(recidivism_path(), show_col_types = FALSE) |>
    filter(!is.na(recid_rate), !is.na(releases))
}

### release-weighted 3-year reincarceration rate per cohort, for every state
### reporting that cohort and for a fixed set that reports every cohort in
### panel_years, so changes in which states report don't drive the trend
recidivism_trend <- function(recid, panel_years = 2010:2020) {
  panel_states <- recid |>
    filter(year %in% panel_years) |>
    count(state_name) |>
    filter(n == length(panel_years)) |>
    pull(state_name)
  pooled <- function(df, label) {
    df |>
      group_by(year) |>
      summarise(value = 100 * sum(recid) / sum(releases), states = n(), .groups = "drop") |>
      mutate(series = label)
  }
  bind_rows(
    pooled(recid, "All reporting states"),
    pooled(filter(recid, state_name %in% panel_states, year %in% panel_years),
           paste0("Same ", length(panel_states), " states each year"))
  )
}

### the 50 states, 1 goal comparison: each state's 2016 rate (2017 when 2016 is
### missing) against its end_year rate, for states with both, release-weighted
recidivism_baseline_change <- function(recid, end_year = 2021) {
  states <- recid |>
    filter(year %in% c(2016, 2017, end_year)) |>
    group_by(state_name) |>
    filter(any(year == end_year), any(year %in% c(2016, 2017))) |>
    filter(year == end_year | year == min(year)) |>
    mutate(period = if_else(year == end_year, "end", "baseline")) |>
    ungroup()
  by_state <- states |>
    select(state_name, period, recid_rate) |>
    pivot_wider(names_from = period, values_from = recid_rate)
  pooled <- states |>
    group_by(period) |>
    summarise(value = 100 * sum(recid) / sum(releases), states = n(), .groups = "drop")
  list(pooled = pooled, fell = sum(by_state$end < by_state$baseline),
       rose = sum(by_state$end > by_state$baseline))
}

### slide 10: probation ----------------------------------------------------------

### people on probation at year end; bjs probation and parole tables from 2010,
### council on criminal justice's compilation of the same bjs series before that
us_probation <- function(pop, from = first_year) {
  bjs <- read_csv(jr_data_path("analysis", "bjs", "ppus", "adults_on_probation_by_jurisdiction.csv"),
                  show_col_types = FALSE) |>
    filter(geo_name == "United States") |>
    select(year, probation = `Probation population on December 31st`)
  ccj <- read_csv(jr_data_path("raw", "ccj", "correctional_control", "ccj_probation_1980_2023.csv"),
                  show_col_types = FALSE) |>
    select(year, probation = `Number of People on Probation`) |>
    filter(!year %in% bjs$year)
  bind_rows(ccj, bjs) |>
    filter(year >= from) |>
    arrange(year) |>
    left_join(pop, by = "year") |>
    mutate(rate = 1e5 * probation / pop_adult)
}

### slides 13, 14: staffing and cost ------------------------------------------------

### inflation index from R/page-data.R, as a multiplier into to_year dollars
inflation_multiplier <- function(to_year) {
  idx <- suppressWarnings(read_inflation_index())
  transmute(idx, year, multiplier = idx$inflation_index[idx$year == to_year] / inflation_index)
}

### correctional officer pay and workload from bls oews (all employers, so
### local jails are in there too). bls skips a few states some years, so both
### measures only use states with numbers that year: pay is a job-weighted mean,
### and people per officer matches each state's officers to its own prison count
us_correctional_officers <- function(nps, prisoners_2023) {
  prison <- nps |>
    filter(!state_abbr %in% c("ST", "US", "FE")) |>
    select(year, state_abbr, prison_pop) |>
    bind_rows(select(prisoners_2023, year, state_abbr, prison_pop))
  read_rds(jr_data_path("analysis", "bls", "oews", "bls_oews_state.rds")) |>
    filter(group == "Correctional Officers and Jailers",
           indicator %in% c("Total employment", "Mean annual wage")) |>
    ### the library file's 2019 rows have no state abbreviation, so go by name
    mutate(state_abbr = coalesce(as.character(state_abbr), state.abb[match(state_name, state.name)],
                                 if_else(state_name == "District of Columbia", "DC", NA))) |>
    select(year, state_abbr, indicator, n) |>
    pivot_wider(names_from = indicator, values_from = n) |>
    filter(!is.na(`Total employment`), !is.na(`Mean annual wage`)) |>
    left_join(prison, by = c("year", "state_abbr")) |>
    group_by(year) |>
    summarise(
      mean_wage = weighted.mean(`Mean annual wage`, `Total employment`),
      per_officer = sum(prison_pop[!is.na(prison_pop)]) / sum(`Total employment`[!is.na(prison_pop)]),
      states = n(),
      .groups = "drop"
    ) |>
    ### no prison counts by state past 2023
    mutate(per_officer = if_else(year > 2023, NA_real_, per_officer))
}

### average yearly pay for a few oews occupations, a job-weighted mean across
### states (bls only publishes state files here, no national one). each year
### keeps the states with both a job count and a wage
us_oews_pay <- function(groups = c("Correctional Officers and Jailers",
                                   "First-Line Supervisors of Correctional Officers",
                                   "Total")) {
  read_rds(jr_data_path("analysis", "bls", "oews", "bls_oews_state.rds")) |>
    filter(group %in% groups, indicator %in% c("Total employment", "Mean annual wage")) |>
    select(year, state_name, group, indicator, n) |>
    pivot_wider(names_from = indicator, values_from = n) |>
    filter(!is.na(`Total employment`), !is.na(`Mean annual wage`)) |>
    group_by(year, group) |>
    summarise(mean_wage = weighted.mean(`Mean annual wage`, `Total employment`),
              jobs = sum(`Total employment`), states = n(), .groups = "drop")
}

### nasbo state corrections spending (operating, all fund sources, millions),
### by state fiscal year
read_nasbo_corrections <- function() {
  readxl::read_excel(jr_data_path("raw", "nasbo", "se", "1991-2022 State Expenditure Report Data.xlsm"),
                     sheet = "STATE EXP REPORT DATA") |>
    select(year = YEAR, state_name = 2, corrections = CORR_TOT) |>
    mutate(corrections = as.numeric(corrections) * 1e6) |>
    filter(!is.na(year))
}

### slide 16: map -----------------------------------------------------------------

### hex grid shared with the site's maps
read_hex_states <- function() {
  sf::read_sf("data/us_states_hexgrid.geojson") |>
    select(state_abbr = iso3166_2)
}

### split states into thirds; low values are leading for every measure here
tercile_status <- function(x) {
  cut(x, quantile(x, c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE), include.lowest = TRUE,
      labels = c("Leading", "Middle", "Lagging"))
}

status_colors <- c(
  Leading = site_colors[["blue"]], Middle = site_colors[["gray_light"]],
  Lagging = site_colors[["orange"]], `No data` = "white"
)
