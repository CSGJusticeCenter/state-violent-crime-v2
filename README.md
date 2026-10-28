## State violent crime interactive pages

Quarto site with one page per state plus DC (`state-viol-crime.qmd`), a backlog-charts-only page per state plus DC (`state-backlog-viz-only.qmd`) and a national page (`index.qmd`). Pages render to `_site/`, which is uploaded to Netlify from a local machine. `_site/` is not committed.

Site: https://csg-state-violent-crime.netlify.app/ (password-protected; ask the team for the password)

### Setup

- R 4.6.1. Packages are managed with renv, which activates when R starts in the project root.
- Run `renv::restore()` once after cloning to install the locked package versions.
- Data comes from the CSG SharePoint `jr_data_library`, read through `jr_data_path()` (which wraps `csgjcr::csg_sp_path()`), so SharePoint must be synced locally.
- Deploying needs access to the site in the CSG Netlify team and the Netlify CLI. Install the CLI with Node.js and log in once:

```sh
npm install -g netlify-cli
netlify login
```

### Build

1. `R/01-prep-agency-county-data.R` builds the county map, agency tables and state reporting coverage in `data/` from the CDE agency file in `jr_data_library`. It also writes the clearance screen (`data/clearance_screen.rds`) and New York's repaired 2022 clearance rates (`data/clearance_repairs.rds`), and prints the state-years the screen flags. Rerun when that file refreshes. State pages stop rendering when the agency year differs from the latest year in the state estimates.
2. `R/02-render-site.R` empties `_site/`, renders the 51 state pages, the 51 backlog pages and the national page four at a time, then copies `styles.css`, `img/` and `fonts/` into `_site/`. It prints each page's render time. A page that fails is retried once, and a second failure stops the script. Each failure prints the exit status, the last knitr chunk reached and Quarto's output.
   - `Rscript R/02-render-site.R NY US BACKLOG-NY` renders only the listed pages. Unknown page names stop the script before `_site/` is touched.
   - Set `n_parallel <- 1` at the top of the script to render one page at a time.
   - After rendering, it warns about NA, NaN or Inf in page text and in `alt`, `title` and `aria-label` attributes. Chart and table contents are not checked, because widget data holds many legitimate NA values.

Run from the project root, in the R console or with `Rscript R/02-render-site.R`. The pages also read state-level SRS, SHR, LEE and ASSLGF files directly from `jr_data_library`.

### Deploy

Pushing to GitHub does not update the site. Netlify builds are stopped, and `_site/` is uploaded from a local machine with `R/03-deploy-site.R`. The script picks the target from the current git branch.

| Branch | Deploys to |
|---|---|
| `main` | Live site, then deletes previews of branches no longer on GitHub |
| Any other | Preview at `<branch>--csg-state-violent-crime.netlify.app` |

Workflow for an update:

1. Make changes on a branch. For quick checks, render one state and open its page in `_site/` in a browser.
2. Run `R/02-render-site.R`, then `R/03-deploy-site.R` to post a preview for review. Rerunning on the same branch updates the same preview URL.
3. Merge the PR.
4. Switch to `main`, pull, run `R/02-render-site.R`, then `R/03-deploy-site.R` to update the live site.

Notes:

- `_site/` is not tracked, so it doesn't change when you switch branches. It holds whatever was rendered last. Always re-render on `main` before a live deploy.
- A preview stays up until its branch is deleted on GitHub and the next live deploy runs. Deleted previews can't be restored, but rerunning the script on the branch makes a new one.
- The script stops if any page is missing from `_site/`. Each deploy replaces the whole site, so a missing page would go offline.
- Both scripts run from the R console or a terminal with `Rscript`, from the project root.
- To roll back, open the site's Deploys list in the Netlify dashboard and publish an earlier deploy.

### Annual update

| Setting | Location |
|---|---|
| Agency year | `R/01-prep-agency-county-data.R` (`agency_year`) |
| Base year for maps, tables and agency changes | `R/page-data.R` (`base_year`); rerun step 1 after changing it |
| SHR pooling start year | `R/page-data.R` (`shr_first_year`) |
| Backlog start year | `R/page-data.R` (`backlog_first_year`) |
| Clearance screen threshold and repaired offenses shown | `R/page-data.R` (`max_no_clearance_share`, `min_screen_reported`, `repaired_offenses_shown`). Pages stop if the screen hides a base, first or latest year; decide on a repair or exclusion before rendering. Pages also stop if the screen lacks a shown year, so raise `agency_year` in step 1 and rerun it when the SRS data adds a year |
| 2021 solve-rate coverage bar | `R/page-data.R` (`min_solve_coverage`); coverage comes from `data/ucr_coverage_2021.csv`, built by `R/download-ucr-coverage.R` |
| Inflation index file | `data/annual-index-value_annual-percent-change_YYYY.xls`, referenced in `R/page-data.R` (`inflation_index_path`) |
| Narrative text | `index.qmd`, `state-viol-crime.qmd`, `_backlog-charts.qmd` and `_methodology-*.qmd` |

The latest SRS year comes from the data, so the state pages pick it up automatically.

### Open questions

- **Illinois 2025 solve rates.** Illinois stays excluded from solve rates because its 2019 clearances are unusable (about 2% of aggravated assaults cleared). Its 2025 clearances look plausible (about 44% for aggravated assault and homicide). Decide whether to show its 2025 solve rate and blank only the change since 2019.
- **Hardcoded claims.** Some narrative claims are typed by hand, so they won't update with new data. Recheck them each year, or generate them from the same data the charts use. All are in `index.qmd`:
  - the US solve rate trend
  - the homicide solve rate comparisons by race, gender and age
  - the NYPD report comparison (within half a percentage point in 2019 and 2020) in the methodology on the US and New York pages
  - the national NIBRS and SRS coverage shares in the methodology, from the FBI's *Reported Crimes in the Nation* report for the year. Update the figures, report name and link. The states with low coverage named after them are generated by step 1 (`data/state_coverage.rds`), so they update when step 1 reruns.

### Handout analysis

`analysis/crime_wo_conseq/` reproduces every number and chart in the *Crime Without Consequence* handout, pages 1–4. The scripts read the same SharePoint data and `R/page-data.R` helpers as the site. They don't feed the site. Run them in order from the project root.

| Script | Does | Writes |
|---|---|---|
| `01-national-series.R` | US reported, solved and unsolved violent crime, 1965–2025. Solved counts use the site method from 2015 and rebuild it from agency counts before that. Prints checks comparing the two methods | `national-1965-2025.csv` |
| `02-state-2020-2025.R` | State backlog by year, 2020–2025, for violent crime and each offense: reported, solved, unsolved, share solved and crime rate per 100,000, pooled from 2020. The 2025 rows are the handout's state table | `state-backlog-2020-2025.csv` |
| `03-handout-numbers.R` | Computes each handout number by page, with the handout text quoted beside its code, and prints them | Nothing |
| `04-figures.R` | ggplot versions of the handout's charts, for reference | `p1-*.png` to `p4-*.png` |
| `05-backlog-export.R` | US backlog by year, 2020–2025, for violent crime and each offense | `us-backlog-2020-2025.csv` |

`helpers.R` holds the shared paths and calculations. Earlier versions of these scripts and their outputs are in `_archive/crime_wo_conseq/`.

### Layout

| Path | Contents |
|---|---|
| `R/utils.R` | Highcharter theme, chart and table helpers |
| `R/page-data.R` | Shared settings and data prep for both pages |
| `_quarto.yaml` | Project settings and page format options shared by both pages |
| `_methodology-*.qmd` | Methodology text included in both pages |
| `R/agency-county-data.R` | Agency and county aggregation used by step 1 |
| `R/local-map-data.R` | County map loading, including the merged NYC feature |
| `R/download-county-maps.R` | Refreshes `maps/` from the pinned Highcharts map collection |
| `R/download-nypd-clearance.R` | Refreshes `data/nypd_clearance_2022.csv`, NYPD's 2022 citywide clearance rates, the average of its four quarterly Local Law 42 reports |
| `R/cde-api.R` | CDE API helper for the download scripts |
| `R/download-ucr-coverage.R` | Refreshes `data/ucr_coverage_2021.csv`, 2021 UCR population coverage by state from the CDE API. Needs `DATA_GOV_API_KEY` |
| `R/04-export-data.R` | Writes US and state CSVs of reported, solved and unsolved violent crime and solve rates to `exports/` |
| `data/` | Prepped agency data and state reporting coverage, CSG regions, inflation index, US hex grid, and `ucr_participation_1960_2025.csv` (UCR agency participation by state and year from the [CDE downloads page](https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads), for reference; the pages don't read it) |
| `maps/` | Highcharts county maps (see `maps/README.md`) |
| `img/`, `fonts/`, `styles.css` | Site assets |
| `tests/` | testthat tests for the helpers |
| `analysis/` | Analysis behind CSG publications that use this data (see Handout analysis) |
| `_archive/` | Retired pipeline scripts, one-off requests and old outputs |

### Tests

```r
testthat::test_dir("tests")
```
