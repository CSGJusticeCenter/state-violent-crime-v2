## render 51 state violent crime pages and copy to _site for netlify deploy

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

# copy csg logo to site folder (prob is already there)
file.copy(
  from = "logo.png",
  to = "_site/logo.png",
  overwrite = TRUE
)

# copy fonts to site folder
file.copy(
  from = "fonts/",
  to = "_site/",
  recursive = TRUE,
  overwrite = TRUE
)
 

# file.copy(
#   from = "index.html",
#   to = "_site/index.html",
#   overwrite = TRUE
# )
#
# file.copy(
#   from = "state-viol-crime-home_files/",
#   to = "_site/",
#   recursive = TRUE,
#   overwrite = TRUE
# )
#
# unlink("index.html")
# unlink("state-viol-crime-home_files/", recursive = TRUE)

# # define function to copy all html files to _site folder
# copy_html_to_site <- function(state) {
#   file.copy(
#     from = paste0("state-viol-crime-", tolower(state), ".html"),
#     to = paste0("_site/state-viol-crime-", tolower(state), ".html"),
#     overwrite = TRUE
#     )
#   }
#
# # iterate over states and copy
# purrr::walk(c(state.abb, "DC"), copy_html_to_site)
#
# # define function to delete html files in home dir
# delete_html <- function(state) {
#   unlink(paste0("state-viol-crime-", tolower(state), ".html"))
# }
#
# # iterate over states and delete
# purrr::walk(c(state.abb, "DC"), delete_html)
#
# # make sure to copy folder with js deps to _site
# file.copy(
#   from = "state-viol-crime_files/",
#   to = "_site/",
#   recursive = TRUE,
#   overwrite = TRUE
# )
#
# # delete js _deps
# unlink("state-viol-crime_files/", recursive = TRUE)
