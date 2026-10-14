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
| Everything below | Open |

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

### 1.2 County map

- Add a legend entry for gray counties: "Fewer than 25 incidents / no data."
- Add the source caption. It's the only chart without one.
- Turn off `credits`. The "Highcharts.com © USA Census Bureau" line appears only on maps.
- Bin the color scale into 5 classes with `dataClasses` so readers can read values from the legend.
- Move the hard-coded title style in `state-viol-crime.qmd` into the theme.

### 1.3 U.S. hex maps

- Cap the symmetric change scale at a high percentile, or bin it with `dataClasses`.
  - The largest absolute value sets the scale now, so one outlier pales every other state.

### 1.4 Homicide panels

- Give the state series the strong color and the region series a muted gray.
  - Region is dark navy and state is lighter teal now, so the comparison draws the eye.
  - The trend charts already treat the U.S. this way.

### 1.5 All charts

- Add `"downloadCSV"` and `"viewData"` to the export menu with `modules/export-data.js`. A few lines in `hc_setup()`.
- Make axis labels larger and darker.

## 2. Tables

### 2.1 Headers

- Use one case style. Headers mix Title Case and sentence case, and "Agency" is styled differently.
- Group columns with reactable `columnGroups`.
  - Overall table: "2025" over rate, incidents and solve rate; "Change since 2019" over the change column.
  - By-offense table: one group per offense with "Per 100k" and "Solve rate." Roughly halves header height and drops the `<br><br>` in `solve_rate_column()`.

### 2.2 Benchmark row

- Move the state or U.S. rate out of each solve-rate header into a `footer` row labeled "State total."

### 2.3 Missing values

- Show "—" instead of blank cells, with a note under the table.

### 2.4 Alignment

- Right-align every number column. Homicide is centered and rape is left-aligned in `solve_rate_layout`.

### 2.5 Filtering and paging

- Add `filterable = TRUE` on County, or a select filter. NY has over 400 agencies across 40+ pages.
- Default to 25 rows per page.

### 2.6 Fonts and packages

- Use GT America in tables and drop the Google Fonts import.
  - `reactable_template()` asks for Source Sans Pro at weight 400; `styles.css` loads only 600.
- Use `reactablefmtr` (e.g., `data_bars()` on solve rate or population) or drop it. Both pages load it and neither calls it.

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

### 3.3 Methodology links

- Give the methodology callout an `#id` and link the five "see the data sources and methodology section below" mentions to it.

### 3.4 Titles

- Replace "and arrests" in state page titles. The pages measure clearances, which include exceptional clearances.
- Switch the U.S. page H1 to sentence case to match other headings.

### 3.5 Resources section

- Cut the state page's four paragraphs to one short paragraph per tab, as the U.S. page does.
  - The current text covers spending and officers per 1,000 crimes but not officers per capita.

## 4. Not yet checked

- Phone-width layout. Tab rows on the crime section (6 tabs) and wide tables need a look at 375px.
