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
| Legends as labels only, in all charts (1.5), PR #29 | Done |
| CSV and data-table export menu items (1.5) | Declined (data will be published elsewhere) |
| Review fixes on PR #29: white no-data counties, map screen-reader text, staffing prose, U.S. page title | Done |
| Hollow 2021 marker note (1.1), binned hex change maps (1.3), muted region bars (1.4), in-page TOC (3.1) | Done on `design-trends-maps` |
| Chart titles as findings (1.1), larger axis labels (1.5) | Declined (keep as is) |
| Summary row (3.2) | On hold, pending other edits |
| Direct labels, shared tooltip, "By offense" chart (1.1) | Mocked, awaiting a decision |
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
| Breaks | Split the 1,890 counties with 25 or more incidents in 2025 into groups of similar size; the same in every state |
| Settings | `county_map_breaks`, `county_map_colors`, `county_map_null_color` and `county_map_border` in `R/page-data.R`; `map_data_classes()` in `R/utils.R`, with a test |
| Rounding | `solve_rate_percent()`, the same rounding as the tables, so a county's class matches its tooltip |
| Counties without a rate | White with the black border. The old light gray was nearly identical to the lightest class (1.02:1 contrast) |
| No-rate legend entry | "Fewer than 25 incidents or no data", drawn by an empty map series |
| Screen readers | Each county reads its rate, "not shown, fewer than 25 incidents" or "no agencies reported data" |
| Point descriptions | `hc_setup()` describes every point on all charts. Highcharts skips charts with more than 200 points by default, which silenced the Texas map |
| Caption | "FBI UCR Program, agency crime and clearance data, [year]" |
| Credits | Off in `hcmap_local()` |
| Title style | Comes from `hc_theme_jc`; the copies in the county map and `state_map_chart()` are gone |
| Tooltip | The map's own tooltip formatter is gone, because `hc_setup()` replaced it anyway |
| Legend symbols | Squares in place of dots |

- Hollow 2021 markers: done. `hc_hollow_year()` adds "Hollow points mark 2021, when fewer agencies reported." above the caption, only when a 2021 point is shown.
- Mockups of direct labels, shared tooltips and "By offense" alternatives (stacked area, share columns, per-1,000 grid) use New York data.

### 1.3 U.S. hex maps — done

| Change | Result |
| --- | --- |
| Classes | Seven fixed `dataClasses`, symmetric around zero |
| Breaks | Crime rate 5, 15, 30 percent; solve rate 2, 5, 10 points (`crime_change_breaks`, `solve_change_breaks` in `R/page-data.R`) |
| Why | One state's change set the old scale (−41% crime, +16 points solved), so most states looked pale |
| Rounding | `change_map_class()` rounds first, so -5 and +5 both leave the middle class and match the tooltip |
| Labels | Signed whole numbers, e.g., "-29% to -15%" (`change_map_data_classes()`) |
| Legend | Squares; the white middle class gets a gray outline |
| Screen readers | Read `change_text`, e.g., "+5%" |

### 1.4 Homicide panels — done

- State bars use the trend charts' state teal; region bars use gray `#8C8C8C` (3.4:1 on white). `shr_series_colors` in `R/utils.R`.

### 1.5 All charts

- Add `"downloadCSV"` and `"viewData"` to the export menu with `modules/export-data.js`. Declined, since the data will be published elsewhere.
- Make axis labels larger and darker. Declined.
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

### 3.1 Navigation — done

- `toc: true`, `toc-location: body`, `toc-depth: 2` in `_quarto.yaml`.
- The CSG site embeds pages in a cross-origin iframe 6,000px tall. Pages fit inside it, so the iframe never scrolls, and Chrome won't let an anchor jump inside it scroll the parent page.
- A script in `_quarto.yaml` handles every in-page link with `scrollIntoView()`, which does scroll the parent. Tested in a local cross-origin harness that copies the CSG embed.
- The methodology links from PR #29 had the same problem in the embed and now use the same script.

### 3.2 Summary row — on hold

- Add four stats under the state page title.
  - 2025 crime rate
  - Comparison with the U.S.
  - Solve rate
  - Solve-rate change since 2019
- A small CSS grid in `styles.css` is enough.

### 3.3 Methodology links — done in PR #29

- The methodology callout on both pages has the id `#methodology`.
- All five "see the data sources and methodology section below" mentions link to it. The U.S. page's "data and methodology sections" now uses the same wording.
- A script in `_quarto.yaml` (`include-after-body`) opens the collapsed callout when one of these links is clicked, and when a page loads or navigates to `#methodology`.

### 3.4 Titles — done in PR #29

- State page titles read "[State] data on violent crime and solve rates". Pages without solve-rate data still read "[State] data on violent crime".
- The U.S. page H1 reads "Trends in violent crime and accountability".
- The U.S. page `<title>` uses the same text through `pagetitle`. It read "index" before.

### 3.5 Resources section — done in PR #29

- A two-sentence intro sits above the tabs and says spending and staffing may show the level of policing.
- Officer sentences on both pages use past tense ("decreased by 6 percent from 2019 to 2025") to match the spending sentence.
- Each tab has one paragraph, as on the U.S. page.
  - Expenditures: inflation-adjusted change since 2019 and what spending includes
  - Officers per capita: new sentence on the change in officers per 10,000 residents since 2019 (`pct_change_staffing`)
  - Officers per violent offense: change since 2019 and the likely cause

## 4. Not yet checked

- Phone-width layout. Tab rows on the crime section (6 tabs) and wide tables need a look at 375px.
