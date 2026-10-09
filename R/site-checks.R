## checks run on rendered pages in _site

# NA, NaN and Inf in a page's visible text, with surrounding context
# script and style contents are skipped, so widget data isn't scanned
find_bad_values <- function(html_file, context = 40) {
  text_nodes <- xml2::read_html(html_file) |>
    xml2::xml_find_all("//body//text()[not(ancestor::script) and not(ancestor::style)]")

  text <- xml2::xml_text(text_nodes) |>
    stringr::str_squish()
  text <- text[nzchar(text)]

  pattern <- "-?\\b(NA|NaN|Inf)\\b"
  hits <- text[stringr::str_detect(text, pattern)]

  found <- purrr::map(hits, \(x) {
    loc <- stringr::str_locate_all(x, pattern)[[1]]
    tibble::tibble(
      value = stringr::str_sub(x, loc[, "start"], loc[, "end"]),
      context = stringr::str_sub(x, pmax(loc[, "start"] - context, 1), loc[, "end"] + context)
    )
  })

  dplyr::bind_rows(
    tibble::tibble(value = character(), context = character()),
    found
  ) |>
    dplyr::mutate(file = basename(html_file), .before = 1)
}
