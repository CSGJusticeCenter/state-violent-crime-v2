## upload the rendered _site folder to netlify
## on main this updates the live site and deletes previews of branches that no
## longer exist on github; on any other branch it makes a preview

netlify_site <- "csg-state-violent-crime"
netlify_site_id <- "73896f77-5251-423c-9da5-c47f6603905f"

# netlify is a .cmd shim on windows, which needs cmd to run it
netlify <- function(args, ...) {
  if (.Platform$OS.type == "windows") {
    system2("cmd", c("/c", "netlify", args), ...)
  } else {
    system2("netlify", args, ...)
  }
}

# call a netlify api method and parse the json it returns
netlify_api <- function(method, data) {
  data_json <- gsub('"', '\\\\"', jsonlite::toJSON(data, auto_unbox = TRUE))
  out <- netlify(c("api", method, "--data", paste0('"', data_json, '"')), stdout = TRUE)
  if (!is.null(attr(out, "status"))) stop("netlify api ", method, " failed")
  jsonlite::fromJSON(paste(out, collapse = "\n"))
}

branch_alias <- function(branch) gsub("[^a-z0-9]+", "-", tolower(branch))

# delete branch and pull request previews whose branch is gone from github
delete_stale_previews <- function() {
  remote_heads <- system2("git", c("ls-remote", "--heads", "origin"), stdout = TRUE)
  remote_branches <- sub(".*refs/heads/", "", remote_heads)
  live_branches <- c(remote_branches, branch_alias(remote_branches))

  deploys <- list()
  page <- 1
  repeat {
    batch <- netlify_api(
      "listSiteDeploys",
      list(site_id = netlify_site_id, page = page, per_page = 100)
    )
    if (length(batch) == 0) break
    deploys[[page]] <- batch[c("id", "context", "branch")]
    if (nrow(batch) < 100) break
    page <- page + 1
  }
  deploys <- do.call(rbind, deploys)

  stale <- deploys[
    deploys$context %in% c("branch-deploy", "deploy-preview") &
      !deploys$branch %in% live_branches, ]

  if (nrow(stale) == 0) {
    message("No stale previews to delete")
    return(invisible())
  }

  for (i in seq_len(nrow(stale))) {
    message("Deleting ", stale$context[i], " for ", stale$branch[i], " (", stale$id[i], ")")
    netlify_api("deleteDeploy", list(deploy_id = stale$id[i]))
  }
}

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
is_main <- identical(branch, "main")

deploy_args <- c(
  "deploy",
  "--dir=_site",
  "--no-build",
  paste0("--site=", netlify_site),
  paste0("--message=\"", branch, " ", commit, "\"")
)

# previews get a stable url per branch: <alias>--csg-state-violent-crime.netlify.app
if (is_main) {
  deploy_args <- c(deploy_args, "--prod")
} else {
  deploy_args <- c(deploy_args, paste0("--alias=", branch_alias(branch)))
}

status <- netlify(deploy_args)

if (status != 0) stop("netlify deploy failed with status ", status)

if (is_main) delete_stale_previews()
