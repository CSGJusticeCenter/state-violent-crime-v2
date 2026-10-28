# AGENTS.md

Quarto site of violent crime and solve-rate pages for each state, DC and the US. See `README.md` for the build steps and layout.

## Running code

- Start R from the project root. `.Rprofile` activates renv there; starting elsewhere loads the wrong library and breaks root-relative paths.
- Don't pass `--vanilla` or `--no-init-file` unless you mean to bypass renv.
- Run tests with `Rscript -e 'testthat::test_dir("tests")'`. Tests source `../R/*.R` and need no SharePoint data.
- Full render takes a while. To check a change, render one state and the US page:

```r
quarto::quarto_render("state-viol-crime.qmd", execute_params = list(state = "NY"),
                      output_file = "state-viol-crime-ny.html", quiet = TRUE)
quarto::quarto_render("index.qmd", output_file = "index.html", quiet = TRUE)
```

- Don't run a bare project-wide `quarto render`. `R/02-render-site.R` is the supported path.

## Data

- Source data lives in SharePoint `jr_data_library`, read with `jr_data_path(...)` from `R/page-data.R`, which finds the library folder in any capitalization under `CSG_SP_PATH`. If a path fails, ask before guessing a new one.
- `data/csg-regions.csv` and the inflation `.xls` are vendored. Their old SharePoint sources moved to `z_ARCHIVE`.

## Site output and deploy

- `_site/` is git-ignored build output. Pushing to GitHub does not deploy, because Netlify builds are stopped.
- `R/02-render-site.R` empties `_site/` before rendering. A one-state test render on a fresh checkout leaves `_site/` with only that page.
- `R/02-render-site.R` renders pages in parallel, each in a temporary copy of the project, because Quarto stages widget libraries in a shared root `libs/` folder. `make_slot()` copies everything except dot files, `_site/`, `_archive/`, `renv/`, `libs/` and `*_files/`. A page that reads from one of those folders works in a one-page render from the root but fails in 02.
- Render processes run with renv's sandbox and lockfile check off. The sandbox lock stalls parallel renders for minutes. These settings go only to the render processes, not the calling R session.
- `R/03-deploy-site.R` checks that every page exists, then uploads `_site/` with the Netlify CLI. On `main` it updates the live site, then deletes branch and PR previews whose branch is gone from GitHub. On other branches it posts a preview at `<branch>--csg-state-violent-crime.netlify.app`.
- Never run the deploy script on `main`. Ask before running it for a preview, because preview URLs are reachable outside the team.
- Pages and chart exports load the logo from `img/csgjc-logo.png` on the live site.

## County map rules

- Single-county agencies use their CDE county.
- Multi-county agencies use the county with the largest share of the agency's place or county subdivision population (PEP 2025), falling back to the largest listed county. `county_fips_all` keeps every county.
- NYC boroughs merge into one map feature (`36NYC`) at load time in `R/local-map-data.R`. No other city is merged.
- Highcharts maps lack AK 02063 and VA 51678, so those agencies appear in tables but not on the map.
- Solve-rate change compares each agency with itself against base year 2019. It shows a dash unless the agency has full reporting in both years.

## Backlog chart rules

- Build the backlog charts with `backlog_charts()` on `add_backlog()` output. Both state page types include `_backlog-charts.qmd`; `index.qmd` calls `backlog_charts()` directly.
- The backlog sums unsolved crime from `backlog_first_year`. Years and offenses whose solve rate the site hides add nothing, and the chart caption names them and says the backlog is a minimum.
- State totals come from `backlog_totals()`, the sum of the offense rows, so the total line always equals the stacked offenses. `backlog_charts()` stops the render if they differ by more than 0.5%. The U.S. total uses the pooled solve rate and differs from its offenses by about 0.1%.
- Pages without solve-rate data show `no_solve_data_text` in place of the charts.

## Table rules

- Build tables with `reactable_template()`. Its default cell shows missing values as `na_text`, a dash that screen readers hear as "not available", and sorts them last. Use cell functions, not `colFormat()` or `na`, which can't render that HTML.
- Each table's note lists every reason a cell shows a dash. Update the note when a new reason appears.
- Footer rows hold state or U.S. totals from the state estimates, not sums of agency rows. Page text and footers read the same `total_rate_benchmark()` values, so they always match.
- State pages stop rendering when the agency year differs from the latest estimate year, since the footers and text would then disagree.
- Offense column order comes from `table_offenses` in `R/utils.R`. Select table data with `offense_column_names()`.

## Conventions

- Never delete project files. Move retired code and outputs to `_archive/` with `git mv`. Quarto ignores `_`-prefixed folders.
- Shared chart and table helpers go in `R/utils.R`, with tests in `tests/`.
- Shared data reads, prep and page settings (base year, excluded states, palettes) go in `R/page-data.R`. Every color is named once in `site_colors` there. Source `R/page-data.R` before `R/utils.R`, whose chart themes use those colors.
- Match the existing tidyverse style: native pipe `|>`, snake_case.
- Keep comments short and about present behavior. Don't leave commented-out code; git history has it.
- After adding or upgrading packages, run `renv::snapshot()` and commit `renv.lock`.
- Work on a branch. Don't push or merge without asking.
