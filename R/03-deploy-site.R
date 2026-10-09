## upload the rendered _site folder to netlify
## on main this updates the live site; on any other branch it makes a preview

netlify_site <- "csg-state-violent-crime"

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

branch <- system2("git", c("branch", "--show-current"), stdout = TRUE)
commit <- system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE)

deploy_args <- c(
  "deploy",
  "--dir=_site",
  "--no-build",
  paste0("--site=", netlify_site),
  paste0("--message=\"", branch, " ", commit, "\"")
)

# previews get a stable url per branch: <alias>--csg-state-violent-crime.netlify.app
if (identical(branch, "main")) {
  deploy_args <- c(deploy_args, "--prod")
} else {
  alias <- gsub("[^a-z0-9]+", "-", tolower(branch))
  deploy_args <- c(deploy_args, paste0("--alias=", alias))
}

# netlify is a .cmd shim on windows, which needs cmd to run it
status <- if (.Platform$OS.type == "windows") {
  system2("cmd", c("/c", "netlify", deploy_args))
} else {
  system2("netlify", deploy_args)
}

if (status != 0) stop("netlify deploy failed with status ", status)
