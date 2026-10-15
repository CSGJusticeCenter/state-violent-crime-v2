# Design improvements backlog

Chart, table and page improvements from the October 2026 design audit. All fit the current Quarto, Highcharts and reactable setup.

## Status

| Item | Status |
| --- | --- |
| Flag agencies with implausible zeros | Done |
| Gray small-count solve rates; drop small homicide bars | Done |
| Prose compares against 2019 on both pages | Done |
| Spline to straight lines | Declined (keep splines) |
| Show gaps for hidden years in trend lines | Declined |
| Zero-baseline y-axes | Declined |
| Table cleanup (2.1–2.4, fonts in 2.6), PR #28 | Done |
| County filter and 25 rows per page (2.5) | Declined (search bar covers county; keep 10 rows) |
| County map (1.2), package drop (2.6), methodology links (3.3), titles (3.4), resources text (3.5), PR #29 | Done |
| Legends as labels only (1.5), PR #29 | Done |
| CSV and data-table export (1.5) | Declined (data will be published elsewhere) |
| Everything else below | Open |

## 1. Charts

### 1.1 Trend charts

- Label lines at their last point ("New York 368", "U.S. 329") and drop the legend.
- Set `tooltip = list(shared = TRUE)` on comparison charts so state and U.S. values appear together.
- Write chart titles as findings. `change_phrase()` already produces "fell 4 percent since 2019."
  - Current titles repeat the section heading above them.
- Explain hollow 2021 markers in the caption.
- Drop the "By offense" stacked column chart, or replace it with a share chart.
  - Homicide is about 1% of each stack and can't be seen.
  - Only the bottom segment can be compared across years.
  - The four single-offense tabs already show each trend.

### 1.2 County map — done in PR #29

| Change | Result |
| --- | --- |
| Color classes | Five fixed `dataClasses`: under 35%, 35–45%, 45–55%, 55–65%, 65% or more |
| Breaks | Split the 1,890 mapped counties nationally into groups of similar size; the same in every state |
| Settings | `county_map_breaks` and `county_map_colors` in `R/page-data.R`; `map_data_classes()` in `R/utils.R`, with a test |
| Rounding | Solve rates round half up to whole percents before binning, so a county's class matches its tooltip |
| Gray counties | Legend entry "Fewer than 25 incidents or no data", drawn by an empty map series |
| Caption | "FBI UCR Program, agency crime and clearance data, [year]" |
| Credits | Off in `hcmap_local()` |
| Title style | Comes from `hc_theme_jc`; the copies in the county map and `state_map_chart()` are gone |
| Legend symbols | Squares in place of dots |

### 1.3 U.S. hex maps

- Cap the symmetric change scale at a high percentile, or bin it with `dataClasses`.
  - The largest absolute value sets the scale now, so one outlier pales every other state.

### 1.4 Homicide panels

- Give the state series the strong color and the region series a muted gray.
  - Region is dark navy and state is lighter teal now, so the comparison draws the eye.
  - The trend charts already treat the U.S. this way.

### 1.5 All charts

- Add `"downloadCSV"` and `"viewData"` to the export menu with `modules/export-data.js`. Declined, since the data will be published elsewhere.
- Make axis labels larger and darker.
- Legends label charts only. Done in PR #29.
  - A `load` event in `hc_setup()` cancels clicks on every legend item, so clicks can't hide series or map color classes.
  - Highcharts 10.2 has no chart-wide option for this, and the series option misses color classes.
  - The theme keeps the default cursor and hover color on legend items and turns off legend accessibility, so screen readers don't get show/hide buttons.

## 2. Tables

### 2.1–2.5 Done in PR #28

| Change | Result |
| --- | --- |
| Headers | Sentence case and bold, styled by `reactable_theme` |
| Column groups | "Violent crime, 2025" and "Change since 2019"; one group per offense over "Per 100k" and "Solve rate" |
| Total rows | "NY state total", "DC total" and "U.S. total" footers with population, incidents, rates and changes from the state estimates |
| Page text | Reads the same `total_rate_benchmark()` values as the footers |
| Missing values | Dash read as "not available", sorted last, explained under each table |
| Alignment and widths | Numbers right-aligned; shared widths in `table_widths` |
| Fonts | GT America with tabular figures; Google Fonts import dropped |
| County filter | Built, then removed; the search bar matches counties |
| Page size | Kept at 10 rows |

### 2.6 Packages — done in PR #29

- Dropped `reactablefmtr` from both pages. Neither page called it.
- `renv.lock` drops it and its dependencies: tippy, webshot2, chromote, websocket, AsioHeaders and zip.

## 3. Page structure and copy

### 3.1 Navigation

- Turn on `toc: true` with `toc-location: body` for an in-page jump list that works inside the CSG site iframe.

### 3.2 Summary row

- Add four stats under the state page title.
  - 2025 crime rate
  - Comparison with the U.S.
  - Solve rate
  - Solve-rate change since 2019
- A small CSS grid in `styles.css` is enough.

### 3.3 Methodology links — done in PR #29

- The methodology callout on both pages has the id `#methodology`.
- All five "see the data sources and methodology section below" mentions link to it. The U.S. page's "data and methodology sections" now uses the same wording.
- A script in `_quarto.yaml` (`include-after-body`) opens the collapsed callout when one of these links is clicked.

### 3.4 Titles — done in PR #29

- State page titles read "[State] data on violent crime and solve rates". Pages without solve-rate data still read "[State] data on violent crime".
- The U.S. page H1 reads "Trends in violent crime and accountability".

### 3.5 Resources section — done in PR #29

- A two-sentence intro sits above the tabs and covers why spending and staffing matter.
- Each tab has one paragraph, as on the U.S. page.
  - Expenditures: inflation-adjusted change since 2019 and what spending includes
  - Officers per capita: new sentence on the change in officers per 10,000 residents since 2019 (`pct_change_staffing`)
  - Officers per violent offense: change since 2019 and the likely cause

## 4. Not yet checked

- Phone-width layout. Tab rows on the crime section (6 tabs) and wide tables need a look at 375px.
