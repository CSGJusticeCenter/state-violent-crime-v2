# Refresh data/nibrs_clearance_2025.csv, the outcome of each 2025 violent
# incident for large agencies nationwide: cleared by arrest, cleared by
# exceptional means or not cleared. The FBI's summary counts give only total
# clearances, so the split comes from its incident-level NIBRS files.
# Writes one row per agency and offense, plus a total_label row for all
# violent crime. Each incident counts once, under its most serious offense
# (nibrs_top_offense() in R/page-data.R), so the offense rows sum to the total.
# Downloads each state's file to jr_data_library on first run, which takes a
# while; later runs read the saved files. Needs data/agency_table.rds and
# data/agency_offense_table.rds from R/01-prep-agency-county-data.R.

library(tidyverse)
source("R/page-data.R")

nibrs_year <- 2025
destination <- sprintf("data/nibrs_clearance_%d.csv", nibrs_year)

# the Crime Data Explorer's downloads page links each file through this
# endpoint, which answers with a short-lived link; no API key needed
signed_url_endpoint <- "https://cde.ucr.cjis.gov/LATEST/s3/signedurl?key="

nibrs_dir <- jr_data_path("data", "raw", "fbi", "nibrs", "incident", nibrs_year)

# large agencies from the site's agency tables, as on the agency-level page,
# of any agency type, leaving out the states without reliable clearance data
agency_table <- read_rds("data/agency_table.rds")
agency_year <- agency_table$agency_year[1]
if (agency_year != nibrs_year) stop("data/agency_table.rds covers ", agency_year, ", not ", nibrs_year)
large_agencies <- agency_table |>
  mutate(across(c(state_name, state_abbr), as.character)) |>
  filter(population >= min_city_population, !ori %in% city_agencies_excluded, is.na(data_flag),
         !state_name %in% states_to_exclude_from_solved_rate_viz) |>
  select(ori, agency_name, state_abbr, agency_type, population)
nibrs_states <- sort(unique(large_agencies$state_abbr))

# path to one state's zip, downloaded if it isn't there yet
nibrs_zip <- function(state) {
  path <- file.path(nibrs_dir, sprintf("%s-%d.zip", state, nibrs_year))
  if (file.exists(path)) return(path)

  key <- sprintf("nibrs/incident/%d/%s-%d.zip", nibrs_year, state, nibrs_year)
  link <- jsonlite::fromJSON(paste0(signed_url_endpoint, key))[[key]]
  if (is.null(link)) stop("The Crime Data Explorer has no file for ", key)
  dir.create(nibrs_dir, recursive = TRUE, showWarnings = FALSE)
  # download to a temporary name so an interrupted download is not kept
  partial <- paste0(path, ".part")
  download.file(link, partial, mode = "wb", quiet = TRUE)
  file.rename(partial, path)
  path
}

# the given columns of one table from a state's zip, as text; table and
# column names can differ in case between states, so both match without case
read_nibrs_table <- function(zip, table, columns) {
  files <- unzip(zip, list = TRUE)$Name
  file <- files[tolower(basename(files)) == tolower(paste0(table, ".csv"))]
  if (length(file) != 1) stop(basename(zip), " has no ", table, ".csv")
  exdir <- file.path(tempdir(), basename(zip))
  path <- unzip(zip, files = file, exdir = exdir)
  on.exit(unlink(path))
  read_csv(path, col_types = cols(.default = "c"), col_select = all_of(columns),
           name_repair = tolower, progress = FALSE)
}

# one row per violent incident of the large agencies in a state, with its
# offense, outcome and exceptional clearance reason; an incident with an
# arrest counts as cleared by arrest, since NIBRS allows an exceptional
# clearance only without one
violent_incidents <- function(state) {
  message(state, ": reading")
  zip <- nibrs_zip(state)
  agencies <- read_nibrs_table(zip, "agencies", c("agency_id", "ori")) |>
    filter(ori %in% large_agencies$ori)
  incidents <- read_nibrs_table(zip, "NIBRS_incident",
                                c("incident_id", "agency_id", "incident_date", "cleared_except_id")) |>
    inner_join(agencies, by = "agency_id")
  offenses <- read_nibrs_table(zip, "NIBRS_OFFENSE", c("incident_id", "offense_code")) |>
    filter(offense_code %in% unlist(nibrs_offense_codes), incident_id %in% incidents$incident_id) |>
    summarize(offense = nibrs_top_offense(offense_code), .by = incident_id)
  arrestees <- read_nibrs_table(zip, "NIBRS_ARRESTEE", "incident_id")
  reasons <- read_nibrs_table(zip, "NIBRS_CLEARED_EXCEPT",
                              c("cleared_except_id", "cleared_except_code", "cleared_except_name"))

  incidents |>
    inner_join(offenses, by = "incident_id") |>
    left_join(reasons, by = "cleared_except_id") |>
    mutate(
      outcome = case_when(
        incident_id %in% arrestees$incident_id ~ "arrest",
        cleared_except_code != "N" ~ "exceptional",
        .default = "not_cleared"
      ),
      reason = if_else(outcome == "exceptional", cleared_except_name, NA_character_)
    )
}

# a state whose file is missing or unreadable is reported, not fatal
failed <- character()
incidents <- map(nibrs_states, \(state) tryCatch(violent_incidents(state), error = \(e) {
  message(state, ": skipped, ", conditionMessage(e))
  failed <<- c(failed, state)
  NULL
})) |>
  list_rbind()

if (anyNA(incidents$cleared_except_code)) stop("Some violent incidents have no exceptional clearance code")
if (any(substr(incidents$incident_date, 1, 4) != nibrs_year)) {
  stop("Some incidents fall outside ", nibrs_year)
}
# each incident again under total_label, for the all violent crime rows
incidents <- bind_rows(incidents, mutate(incidents, offense = total_label))

# summary-count totals for comparison, one row per agency and offense
offense_counts <- read_rds("data/agency_offense_table.rds") |>
  select(ori, matches("^n_(reported|solved)_[a-z_]+$"), -starts_with("n_reported_rate")) |>
  pivot_longer(-ori, names_to = c(".value", "key"), names_pattern = "^n_(reported|solved)_(.+)$") |>
  filter(key %in% str_replace_all(str_to_lower(violent_offenses), " ", "_")) |>
  transmute(ori, offense = str_to_sentence(str_replace_all(key, "_", " ")),
            n_srs_reported = reported, n_srs_cleared = solved)
srs_counts <- agency_table |>
  transmute(ori, offense = total_label, n_srs_reported = n_reported, n_srs_cleared = n_solved) |>
  bind_rows(offense_counts)

reason_counts <- incidents |>
  filter(outcome == "exceptional") |>
  count(ori, offense, reason) |>
  mutate(reason = paste0("n_exceptional_", str_replace_all(str_to_lower(reason), "[^a-z]+", "_"))) |>
  pivot_wider(names_from = reason, values_from = n, values_fill = 0)

clearance <- incidents |>
  count(ori, offense, outcome) |>
  pivot_wider(names_from = outcome, values_from = n, names_prefix = "n_", values_fill = 0) |>
  mutate(n_incidents = n_arrest + n_exceptional + n_not_cleared, .after = offense) |>
  left_join(reason_counts, by = c("ori", "offense")) |>
  mutate(across(starts_with("n_exceptional_"), \(x) coalesce(x, 0L)))

rows <- large_agencies |>
  inner_join(clearance, by = "ori") |>
  left_join(srs_counts, by = c("ori", "offense")) |>
  relocate(offense, .after = agency_type) |>
  relocate(n_srs_reported, n_srs_cleared, .before = n_incidents) |>
  mutate(year = nibrs_year, .before = 1) |>
  arrange(desc(population), factor(offense, c(total_label, violent_offenses)))

# agencies left out, which the page's methodology lists
if (length(failed) > 0) message("States skipped: ", paste(failed, collapse = ", "))
missing <- anti_join(large_agencies, clearance, by = "ori")
if (nrow(missing) > 0) {
  message("No NIBRS violent incidents for ", nrow(missing), " agencies:")
  print(select(missing, ori, agency_name, state_abbr, agency_type, population), n = Inf)
}

# the offense rows must sum to each agency's total
sums <- rows |>
  summarize(total = sum(n_incidents[offense == total_label]),
            offenses = sum(n_incidents[offense != total_label]), .by = ori)
if (any(sums$total != sums$offenses)) stop("Offense incidents don't sum to the violent crime total")

# NIBRS follows each incident to its status at the FBI's data freeze and
# counts incidents, while summary counts record clearances made in the year
# and count victims for most offenses, so some gap is expected; a large gap
# in all violent crime can mean an agency reported only part of the year
gaps <- filter(rows, offense == total_label, abs(n_incidents / n_srs_reported - 1) > 0.25)
if (nrow(gaps) > 0) {
  message("NIBRS violent incidents differ from summary counts by more than 25%:")
  print(select(gaps, agency_name, state_abbr, n_incidents, n_srs_reported, n_srs_cleared), n = Inf)
}

write_csv(rows, destination)
message("Wrote ", n_distinct(rows$ori), " agencies in ", n_distinct(rows$state_abbr), " states, ",
        nrow(rows), " rows, to ", destination)
