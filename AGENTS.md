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

- Source data lives in SharePoint `jr_data_library`, read with `csgjcr::csg_sp_path("jr_data_library", ...)`. If a path fails, ask before guessing a new one.
- `fbi_srs_agency.rds` (CDE agency file) feeds `R/01-prep-agency-county-data.R`, which writes `data/county_map.rds`, `data/agency_table.rds` and `data/agency_offense_table.rds`.
- The pages read `fbi_srs_estimated_crimes_state.rds`, `fbi_shr_state.rds`, `fbi_lee_state.rds` and `census_asslgf_state.rds` directly.
- `data/csg-regions.csv` and the inflation `.xls` are vendored. Their old SharePoint sources moved to `z_ARCHIVE`.

## Site output and deploy

- `_site/` is git-ignored build output. Pushing to GitHub does not deploy, because Netlify builds are stopped.
- `R/02-render-site.R` empties `_site/` before rendering. A one-state test render on a fresh checkout leaves `_site/` with only that page.
- `R/02-render-site.R` renders pages in parallel, each in a temporary copy of the project, because Quarto stages widget libraries in a shared root `libs/` folder. If a page starts reading a new local file or folder, add it to the copy list in `make_slot()`.
- `R/03-deploy-site.R` checks that every page exists, then uploads `_site/` with the Netlify CLI. On `main` it updates the live site, then deletes branch and PR previews whose branch is gone from GitHub. On other branches it posts a preview at `<branch>--csg-state-violent-crime.netlify.app`.
- Never run the deploy script on `main`. Ask before running it for a preview, because preview URLs are reachable outside the team.
- Pages and chart exports load the logo from `img/csgjc-logo.png` on the live site.

## County map rules

- Single-county agencies use their CDE county.
- Multi-county agencies use the county with the largest share of the agency's place or county subdivision population (PEP 2025), falling back to the largest listed county. `county_fips_all` keeps every county.
- NYC boroughs merge into one map feature (`36NYC`) at load time in `R/local-map-data.R`. No other city is merged.
- Highcharts maps lack AK 02063 and VA 51678, so those agencies appear in tables but not on the map.
- Solve-rate change compares each agency with itself against base year 2019. It's blank when the agency lacks full reporting in both years.

## Conventions

- Never delete project files. Move retired code and outputs to `_archive/` with `git mv`. Quarto ignores `_`-prefixed folders.
- Shared chart and table helpers go in `R/utils.R`, with tests in `tests/`.
- Match the existing tidyverse style: native pipe `|>`, snake_case.
- Keep comments short and about present behavior. Don't leave commented-out code; git history has it.
- After adding or upgrading packages, run `renv::snapshot()` and commit `renv.lock`.
- Work on a branch. Don't push or merge without asking.
