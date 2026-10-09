## State violent crime interactive pages

Quarto site with one page per state plus DC (`state-viol-crime.qmd`) and a national page (`index.qmd`). Pages render to `_site/`, which Netlify deploys on push.

Site: https://csg-state-violent-crime.netlify.app/ (password-protected; ask the team for the password)

### Build

1. `R/01-prep-agency-county-data.R` builds the county map and agency tables in `data/` from the CDE agency file in `jr_data_library`. Rerun when that file refreshes.
2. `R/02-render-site.R` renders all 51 state pages and the national page, then copies `styles.css`, `img/` and `fonts/` into `_site/`.

The pages also read state-level SRS, SHR, LEE and ASSLGF files directly from `jr_data_library`.

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
