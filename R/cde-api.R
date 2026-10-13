# FBI Crime Data Explorer API, summarized endpoint, for the download scripts.
# Needs a data.gov key in DATA_GOV_API_KEY.

cde_api_key <- function() {
  key <- Sys.getenv("DATA_GOV_API_KEY")
  if (!nzchar(key)) stop("Set DATA_GOV_API_KEY to a data.gov API key")
  key
}

# one year of monthly data for a geography path such as "national",
# "state/NY" or "agency/NY0303000"
# retries outages and rate limits, and errors leave the key out of the message
read_cde_summarized <- function(path, offense, year, tries = 6) {
  url <- sprintf(
    "https://api.usa.gov/crime/fbi/cde/summarized/%s/%s?from=01-%d&to=12-%d&API_KEY=%s",
    path, offense, year, year, cde_api_key()
  )
  for (attempt in seq_len(tries)) {
    resp <- tryCatch(curl::curl_fetch_memory(url), error = function(e) e)
    status <- if (inherits(resp, "error")) NA else resp$status_code
    if (!is.na(status) && status == 200) return(jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE))

    # other client errors, such as a bad key, won't clear on retry
    if (!is.na(status) && status >= 400 && status < 500 && status != 429) break
    if (attempt < tries) Sys.sleep(2^attempt)
  }

  problem <- if (is.na(status)) conditionMessage(resp) else paste("HTTP status", status)
  stop(
    "Could not read CDE ", path, " ", offense, " for ", year, ": ",
    gsub(cde_api_key(), "<key>", problem, fixed = TRUE)
  )
}
