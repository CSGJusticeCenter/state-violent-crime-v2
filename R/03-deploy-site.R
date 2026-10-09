## upload the rendered _site folder to netlify

# netlify replaces the whole site on each deploy, so a missing page would go
# offline. stop unless every state page and the US page are present.
expected_pages <- c(
  "index.html",
  paste0("state-viol-crime-", tolower(c(state.abb, "DC")), ".html")
)

missing_pages <- expected_pages[!file.exists(file.path("_site", expected_pages))]

if (length(missing_pages) > 0) {
  stop(
    "_site is missing ", length(missing_pages), " page(s): ",
    paste(missing_pages, collapse = ", "),
    "\nRun R/02-render-site.R before deploying."
  )
}

# deploy the existing _site without re-rendering; site id comes from _publish.yml
status <- system2("quarto", c("publish", "netlify", "--no-render"))

if (status != 0) stop("quarto publish failed with status ", status)
