# FBI Crime Data Explorer API, summarized endpoint, for the download scripts.
# Needs a data.gov key in DATA_GOV_API_KEY.

cde_api_key <- function() {
  key <- Sys.getenv("DATA_GOV_API_KEY")
  if (!nzchar(key)) stop("Set DATA_GOV_API_KEY to a data.gov API key")
  key
}

# one year of monthly data for a geography path such as "national",
# "state/NY" or "agency/NY0303000"
# retries brief outages, and errors leave the key out of the message
read_cde_summarized <- function(path, offense, year, tries = 6) {
  url <- sprintf(
    "https://api.usa.gov/crime/fbi/cde/summarized/%s/%s?from=01-%d&to=12-%d&API_KEY=%s",
    path, offense, year, year, cde_api_key()
  )
  for (attempt in seq_len(tries)) {
    # R's connection warnings repeat the url, key included
    js <- tryCatch(suppressWarnings(jsonlite::read_json(url)), error = function(e) e)
    if (!inherits(js, "error")) return(js)
    Sys.sleep(2^attempt)
  }
  stop(
    "Could not read CDE ", path, " ", offense, " for ", year, ": ",
    gsub(cde_api_key(), "<key>", conditionMessage(js), fixed = TRUE)
  )
}
