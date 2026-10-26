## Solved and unsolved violent crime, 1965-2025, as inline SVG
##
## Draws the long-run chart for page 3 of the Justice Deficit handout from
## analysis/solved-unsolved-since-1960.csv, sized 440 x 293 to fit the page.
## Paste the output in place of the chart's <svg> element in the handout.
## Writes analysis/solved-unsolved-since-1960.svg

library(tidyverse)

d <- read_csv("analysis/solved-unsolved-since-1960.csv", show_col_types = FALSE) |>
  select(year, reported = incidents_reported, solved = incidents_solved)

w <- 440
h <- 293
x_left <- 44
x_right <- 352
y_top <- 52
y_bottom <- 236
y_max <- 2e6

px <- function(year) x_left + (year - 1960) / (2025 - 1960) * (x_right - x_left)
py <- function(v) y_bottom - v / y_max * (y_bottom - y_top)
f1 <- function(x) format(round(x, 1), nsmall = 1, trim = TRUE)
pts <- function(x, y) str_flatten(str_c(f1(x), ",", f1(y)), " ")
fmt_k <- function(v) if_else(v >= 1e6, str_c(format(round(v / 1e6, 2), nsmall = 2), "M"), str_c(round(v / 1e3), "K"))

d <- mutate(d, x = px(year), yr = py(reported), ys = py(solved))
first <- slice_head(d, n = 1)
last <- slice_tail(d, n = 1)
peak_r <- slice_max(d, reported, n = 1)
peak_s <- slice_max(d, solved, n = 1)

font <- "font-family=\"Public Sans, sans-serif\""
navy <- "#001E35"
green <- "#0d8f74"
green_text <- "#0b6e5a"
grey <- "#4b5d6e"

grid <- map_chr(seq(0, 2e6, 5e5), \(v) {
  label <- c("0", "500K", "1M", "1.5M", "2M")[v / 5e5 + 1]
  str_glue(
    "<line x1=\"{x_left}\" x2=\"{x_right + 4}\" y1=\"{f1(py(v))}\" y2=\"{f1(py(v))}\" stroke=\"#d5e0ea\" stroke-width=\"1\"></line>",
    "<text x=\"{x_left - 6}\" y=\"{f1(py(v) + 3.5)}\" text-anchor=\"end\" {font} font-size=\"9.5\" fill=\"{grey}\">{label}</text>"
  )
}) |> str_flatten()

x_labels <- map_chr(seq(1965, 2025, 10), \(yr) {
  str_glue("<text x=\"{f1(px(yr))}\" y=\"{y_bottom + 14}\" text-anchor=\"middle\" {font} font-size=\"9.5\" font-weight=\"700\" fill=\"#0d1b29\">{yr}</text>")
}) |> str_flatten()

dot <- function(x, y, color) str_glue("<circle cx=\"{f1(x)}\" cy=\"{f1(y)}\" r=\"3\" fill=\"#ffffff\" stroke=\"{color}\" stroke-width=\"1.6\"></circle>")
label <- function(x, y, text, color, anchor = "middle", size = 9.5) {
  str_glue("<text x=\"{f1(x)}\" y=\"{f1(y)}\" text-anchor=\"{anchor}\" {font} font-size=\"{size}\" font-weight=\"800\" fill=\"{color}\">{text}</text>")
}

gap <- pts(c(d$x, rev(d$x)), c(d$yr, rev(d$ys)))
mid_gap <- (last$yr + last$ys) / 2

alt <- str_glue(
  "Line chart of violent crime reported and solved in the US, 1965 to 2025. ",
  "Reported crime rose from {fmt_k(first$reported)} in 1965 to a peak of {fmt_k(peak_r$reported)} in {peak_r$year} ",
  "and was {fmt_k(last$reported)} in 2025. Solved crimes rose from {fmt_k(first$solved)} to a peak of ",
  "{fmt_k(peak_s$solved)} in {peak_s$year} and were {fmt_k(last$solved)} in 2025. ",
  "The shaded gap between the lines is crime left unsolved."
)

svg <- str_glue(
  "<svg width=\"{w}\" height=\"{h}\" viewBox=\"0 0 {w} {h}\" role=\"img\" aria-label=\"{alt}\" ",
  "style=\"display: block; width: {w}px; height: {h}px; background: #EEF3F9; border-radius: 10px\">",
  "<text x=\"14\" y=\"24\" {font} font-size=\"13\" font-weight=\"800\" fill=\"#0d1b29\">Violent crime reported and solved in the US, 1965–2025</text>",
  "{grid}",
  "<polygon points=\"{gap}\" fill=\"#7397B0\" fill-opacity=\"0.4\"></polygon>",
  "<line x1=\"{x_left}\" x2=\"{x_right + 4}\" y1=\"{y_bottom}\" y2=\"{y_bottom}\" stroke=\"#0d1b29\" stroke-width=\"1.2\"></line>",
  "<polyline points=\"{pts(d$x, d$yr)}\" fill=\"none\" stroke=\"{navy}\" stroke-width=\"2\" stroke-linejoin=\"round\"></polyline>",
  "<polyline points=\"{pts(d$x, d$ys)}\" fill=\"none\" stroke=\"{green}\" stroke-width=\"2\" stroke-linejoin=\"round\"></polyline>",
  "{dot(first$x, first$yr, navy)}{dot(first$x, first$ys, green)}",
  "{dot(peak_r$x, peak_r$yr, navy)}{dot(peak_s$x, peak_s$ys, green)}",
  "{dot(last$x, last$yr, navy)}{dot(last$x, last$ys, green)}",
  "{label(first$x - 6, first$yr + 3.5, fmt_k(first$reported), navy, 'end')}",
  "{label(first$x - 6, first$ys + 3.5, fmt_k(first$solved), green_text, 'end')}",
  "{label(peak_r$x, peak_r$yr - 7, fmt_k(peak_r$reported), navy)}",
  "{label(peak_s$x, peak_s$ys - 7, fmt_k(peak_s$solved), green_text)}",
  "{label(last$x + 8, last$yr - 1, 'Reported', navy, 'start', 10.5)}",
  "{label(last$x + 8, last$yr + 10, fmt_k(last$reported), navy, 'start', 10.5)}",
  "<text x=\"{f1(last$x + 8)}\" y=\"{f1(mid_gap - 2)}\" {font} font-size=\"9.5\" font-weight=\"700\" fill=\"{grey}\">Unsolved</text>",
  "<text x=\"{f1(last$x + 8)}\" y=\"{f1(mid_gap + 9)}\" {font} font-size=\"9.5\" font-weight=\"700\" fill=\"{grey}\">(shaded gap)</text>",
  "{label(last$x + 8, last$ys - 1, 'Solved', green_text, 'start', 10.5)}",
  "{label(last$x + 8, last$ys + 10, fmt_k(last$solved), green_text, 'start', 10.5)}",
  "{x_labels}",
  "<text {font} font-size=\"7.5\" fill=\"{grey}\">",
  "<tspan x=\"14\" y=\"268\">Solved counts apply clearance rates to estimated violent crimes.</tspan>",
  "<tspan x=\"14\" y=\"279\">Sources: FBI Crime Data Explorer; FBI UCR via Jacob Kaplan; Crime Index, 1965–1978</tspan>",
  "</text></svg>"
)

writeLines(svg, "analysis/solved-unsolved-since-1960.svg", useBytes = TRUE)
