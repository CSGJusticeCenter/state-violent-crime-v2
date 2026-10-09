## render 51 state violent crime pages and the US page to _site for netlify deploy
## Rscript R/02-render-site.R NY IL US renders only those pages, for testing

source("R/site-checks.R")

n_parallel <- 4

pages <- commandArgs(trailingOnly = TRUE)
if (length(pages) == 0) {
  pages <- c("US", state.abb, "DC")
}

# renders run in temporary copies of the project; renv loads this project's library
Sys.setenv(RENV_PROJECT = normalizePath("."))

# renv's lockfile check adds ~2.5 s to every R process a render starts,
# and its sandbox lock stalls parallel renders for up to 5 minutes
Sys.setenv(
  RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE",
  RENV_CONFIG_SANDBOX_ENABLED = "FALSE"
)

# start from an empty _site so old libs and pages don't get deployed
unlink("_site", recursive = TRUE)

# Quarto stages widget libraries in the project's libs/ folder mid-render,
# so parallel renders each need their own copy of the project
make_slot <- function(i) {
  dir <- file.path(tempdir(), paste0("render-slot-", i))
  dir.create(dir)
  file.copy(
    c("_quarto.yaml", "state-viol-crime.qmd", "index.qmd", "styles.css", "R", "data", "maps"),
    dir, recursive = TRUE
  )
  writeLines('source(file.path(Sys.getenv("RENV_PROJECT"), "renv/activate.R"))', file.path(dir, ".Rprofile"))
  dir
}

slots <- purrr::map_chr(seq_len(n_parallel), make_slot)

# start a quarto render process for one page in a free slot
start_render <- function(page, slot) {
  args <- if (page == "US") {
    c("render", "index.qmd", "--output", "index.html")
  } else {
    c(
      "render", "state-viol-crime.qmd",
      "-P", paste0("state:", page),
      "--output", paste0("state-viol-crime-", tolower(page), ".html")
    )
  }

  log <- tempfile(paste0("render-", page, "-"), fileext = ".log")

  list(
    page = page,
    slot = slot,
    log = log,
    start = Sys.time(),
    process = processx::process$new(
      quarto::quarto_path(), args,
      wd = slot, stdout = log, stderr = "2>&1"
    )
  )
}

queue <- pages
running <- list()
timings <- tibble::tibble(page = character(), secs = numeric())
run_start <- Sys.time()

while (length(queue) > 0 || length(running) > 0) {
  free_slots <- setdiff(slots, purrr::map_chr(running, "slot"))

  while (length(free_slots) > 0 && length(queue) > 0) {
    running[[queue[1]]] <- start_render(queue[1], free_slots[1])
    queue <- queue[-1]
    free_slots <- free_slots[-1]
  }

  Sys.sleep(0.2)

  for (job in running) {
    if (job$process$is_alive()) next

    running[[job$page]] <- NULL

    if (job$process$get_exit_status() != 0) {
      purrr::walk(running, \(other) other$process$kill_tree())
      message(paste(tail(readLines(job$log), 20), collapse = "\n"))
      stop(job$page, " page failed to render", call. = FALSE)
    }

    secs <- as.numeric(difftime(Sys.time(), job$start, units = "secs"))
    timings <- tibble::add_row(timings, page = job$page, secs = secs)
    message(sprintf("%s rendered in %.1f s (%d/%d)", job$page, secs, nrow(timings), length(pages)))
  }
}

run_secs <- as.numeric(difftime(Sys.time(), run_start, units = "secs"))
message(sprintf(
  "Rendered %d pages in %.1f minutes, slowest %s at %.1f s",
  length(pages), run_secs / 60,
  timings$page[which.max(timings$secs)], max(timings$secs)
))

# merge each slot's pages and libs into _site
dir.create("_site")
purrr::walk(slots, \(slot) {
  file.copy(list.files(file.path(slot, "_site"), full.names = TRUE), "_site", recursive = TRUE, overwrite = TRUE)
})
unlink(slots, recursive = TRUE)

# copy css stylesheet site folder
file.copy(
  from = "styles.css",
  to = "_site/styles.css",
  overwrite = TRUE
)

# copy csg logo to site folder; pages and chart exports load img/csgjc-logo.png
file.copy(
  from = "img/",
  to = "_site/",
  recursive = TRUE,
  overwrite = TRUE
)

# copy fonts to site folder
file.copy(
  from = "fonts/",
  to = "_site/",
  recursive = TRUE,
  overwrite = TRUE
)

# flag NA, NaN and Inf in page text
bad_values <- list.files("_site", pattern = "\\.html$", full.names = TRUE) |>
  purrr::map(find_bad_values) |>
  purrr::list_rbind()

if (nrow(bad_values) > 0) {
  print(bad_values, n = Inf)
  warning(nrow(bad_values), " NA, NaN or Inf values found in page text", call. = FALSE)
}
