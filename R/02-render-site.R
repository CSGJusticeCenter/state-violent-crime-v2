## render 51 state violent crime pages and the US page to _site for netlify deploy

# start from an empty _site so old libs and pages don't get deployed
unlink("_site", recursive = TRUE)

# define function to render qmd to html and name with state
render_state <- function(state) {
  message("Rendering ", state, " violent crime page")
  quarto::quarto_render(
    input = "state-viol-crime.qmd",
    execute_params = list(state = state),
    output_file = paste0("state-viol-crime-", tolower(state), ".html"),
    quiet = TRUE
    )
}


# iterate over states and render
purrr::walk(c(state.abb, "DC"), render_state)

# us page
quarto::quarto_render(
  input = "index.qmd",
  output_file = "index.html",
  quiet = TRUE
)

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
