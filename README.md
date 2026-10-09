## State violent crime interactive pages

Quarto site with one page per state plus DC (`state-viol-crime.qmd`) and a national page (`index.qmd`). Pages render to `_site/`, which is uploaded to Netlify from a local machine. `_site/` is not committed.

Site: https://csg-state-violent-crime.netlify.app/ (password-protected; ask the team for the password)

### Setup

- R 4.6.1. Packages are managed with renv, which activates when R starts in the project root.
- Run `renv::restore()` once after cloning to install the locked package versions.
- Data comes from the CSG SharePoint `jr_data_library`, read through `csgjcr::csg_sp_path()`, so SharePoint must be synced locally.

### Build

1. `R/01-prep-agency-county-data.R` builds the county map and agency tables in `data/` from the CDE agency file in `jr_data_library`. Rerun when that file refreshes.
2. `R/02-render-site.R` renders all 51 state pages and the national page, then copies `styles.css`, `img/` and `fonts/` into `_site/`.

3. `R/03-deploy-site.R` checks that every page exists in `_site/`, then uploads it with `quarto publish netlify --no-render`. Netlify keeps past deploys, and any of them can be restored from the dashboard's Deploys list.

Run each from the project root, e.g. `Rscript R/02-render-site.R`. The first deploy prompts for a Netlify login and the target site, then saves the site ID in `_publish.yml`. The pages also read state-level SRS, SHR, LEE and ASSLGF files directly from `jr_data_library`.

### Annual update

| Setting | Location |
|---|---|
| Agency year and base year | `R/01-prep-agency-county-data.R` |
| National page base year | `index.qmd` (`base_year`) |
| SHR pooling start year | `index.qmd` and `state-viol-crime.qmd` (`shr_first_year`) |
| Inflation index file | `data/annual-index-value_annual-percent-change_YYYY.xls`, referenced in both `.qmd` files |
| Narrative text | `index.qmd` and `state-viol-crime.qmd` |

The latest SRS year comes from the data, so the state pages pick it up automatically.

### Layout

| Path | Contents |
|---|---|
| `R/utils.R` | Highcharter theme, chart and table helpers |
| `R/agency-county-data.R` | Agency and county aggregation used by step 1 |
| `R/local-map-data.R` | County map loading, including the merged NYC feature |
| `R/download-county-maps.R` | Refreshes `maps/` from the pinned Highcharts map collection |
| `data/` | Prepped agency data, CSG regions, inflation index, US hex grid |
| `maps/` | Highcharts county maps (see `maps/README.md`) |
| `img/`, `fonts/`, `styles.css` | Site assets |
| `tests/` | testthat tests for the helpers |
| `_archive/` | Retired pipeline scripts, one-off requests and old outputs |

### Tests

```r
testthat::test_dir("tests")
```
