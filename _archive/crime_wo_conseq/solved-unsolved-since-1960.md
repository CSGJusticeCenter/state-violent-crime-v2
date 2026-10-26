# Solved and unsolved violent crime, 1965–2025

Notes on the ad-hoc long-run chart of reported, solved and unsolved violent crime in the US. Written 2026-10-15.

## Goal

- Extend the 2020–2025 "Reported violent crime fell, but the number solved did not increase" graphic as far back as possible.
- Follow the site's published method wherever it can run.
- Show counts, and a second version as rates per 100,000 residents.

## Files

| File | What it is |
|---|---|
| `solved-unsolved-since-1960.R` | Builds the series, prints checks, draws both charts |
| `solved-unsolved-since-1960.csv` | One row per year: counts, % solved, population, rates per 100k, sources |
| `solved-unsolved-since-1960.png` | Counts chart |
| `solved-unsolved-per-capita-since-1960.png` | Per 100,000 residents chart |
| `solved-unsolved-since-1960-svg.R` | Draws the counts chart as inline SVG for page 3 of the handout |
| `solved-unsolved-since-1960.svg` | That SVG, 440 x 293 |
| `fact-check-unsolved-murders.R` | Earlier homicide fact check (1985–2025) that this work grew from |

The series starts in 1965. The file names keep "1960".

## Data sources

| Source | Location | Used for |
|---|---|---|
| FBI estimated crimes by state (CDE) | `jr_data_library/data/analysis/fbi/srs/fbi_srs_estimated_crimes_state.rds` via `read_srs_state()` | Reported counts 1979–2025; published method 2015–2025; population 2000–2025 |
| CDE agency file | `jr_data_library/data/analysis/fbi/srs/fbi_srs_agency.rds` | Agency reported and cleared counts 2000–2025 |
| Kaplan Return A, agency-year | `jr_data_library/data/raw/fbi/srs/kaplan/offenses_known_yearly_1960_2024.rds` | Agency reported and cleared counts 1965–1999 |
| Crime Index historical crime | `jr_data_library/data/raw/ahd/crime_index/historical_crime_1930_2025_1790092670.csv` | State reported counts 1965–1978; US population 1965–1999 |
| Crime Index historical clearances | `jr_data_library/data/raw/ahd/crime_index/historical_clearance_1960_2025_1790092670.csv` | Check only: national clearance rates by offense (agency counts also available) |
| NYPD 2022 clearance repair | `data/nypd_clearance_2022.csv` | Same repair the site uses |

### What each source covers

- **FBI estimated crimes file.** Reported counts start in 1979. Clearances start in 1985. Population is missing before 2000.
- **CDE API.** Returns HTTP 400 for every year before 1985 at the national, state and agency levels.
- **Kaplan.** Clearances are zero in 1960–62 and near zero in 1963, so usable clearances start in 1964.
  - Pre-2000 ORIs use old state codes (`NB` for Nebraska), so the script uses Kaplan's `state_abb` column, not the ORI prefix.
  - Florida's 1997 clearances are `NA`, treated as 0.
- **Crime Index** (AH Datalytics, crimeindex.org).
  - National and state tables are hand-digitized from the FBI's *Crime in the United States*, and 2017–2024 are partly filled from their own sample.
  - Both files hold agency rows keyed by 9-character ORI, plus state and US rows. The clearance file has agency reported and cleared counts by offense from 1964, with one US row of rates.
  - Crime Index agency sums run 2–10% below Kaplan's, with about 1,500 fewer agencies a year. It seems to drop agencies with no clearances and some partial reporters.
  - The site method run on Crime Index agencies matches the chart within 0.5 pts in most years, but runs 0.2–0.5 pts below CDE in 2000–2025. Kaplan matches CDE within 0.1 pts, so the chart keeps Kaplan.
  - The crime file has no DC row, so DC is the US total minus the 50 states.
  - The full downloads are 121 MB and 100 MB, too big for GitHub, which is why they live in SharePoint.

## Method

### Reported counts

| Years | Source |
|---|---|
| 1965–1978 | Crime Index state tables |
| 1979–2025 | FBI state estimates |

### Solved counts

| Years | Method |
|---|---|
| 1965–2014 | Site method rebuilt from agency counts |
| 2015–2025 | Published site method, `us_solve_trend()`, with the NYPD 2022 repair |

The site method rebuilt from agency counts works like this:

1. Sum each state's agency reported and cleared counts.
2. Flag non-reporting agencies, those with at least `min_screen_reported` (10) violent offenses and zero clearances.
3. Drop a state-year from the pool when non-reporting agencies hold at least `max_no_clearance_share` (5%) of its reported violent crime. This is the site's clearance screen, applied to every year.
4. Drop Illinois after 1985 (`illinois_last_year`).
5. Pool the remaining states into one national clearance rate.
6. Apply that rate to US reported crime.

### Population

| Years | Source |
|---|---|
| 1965–1999 | Crime Index US population (UCR) |
| 2000–2025 | The site's population, the sum of state `pop_total` on homicide rows |

### Caveats for the chart note

- A year's clearances can include crimes from earlier years.
- The FBI broadened its rape definition in 2013, so reported counts jump a little that year.
- Non-reporting agencies may clear crimes at a different rate than reporting ones.

## Decisions

| Question | Decision | Why |
|---|---|---|
| Start year | 1965 | Agency clearances start in 1964 |
| Method for 1965–2014 | Site method rebuilt from agency counts | Same logic as published; matches published within 0.3 pts in 2015–2025 |
| Switch the site to agency-level exclusion? | No | Changes US numbers by at most 0.3 pts in 2015–2025 |
| Non-reporting threshold | 10 offenses, zero clearances (site's rule) | See the screen counts below |
| Agencies with low but nonzero clearances | Keep them | The cases (DC 1996–97, NY 1995, MI 1988–89) looked reasonable |
| Illinois | Included through 1985, excluded after | Illinois agency rate is 48–56% through 1985, then 42% in 1986 and 18% in 1987 |
| NYPD 2022 | Keep the site's repair | Better than excluding NYPD |
| Coverage floor | None for now | |
| Where the code lives | `analysis/` only | Not on the site for now |

## Findings

### Why the published method can't simply be run back to 1985

`us_solve_trend()` on the FBI estimates from 1985 produces breaks:

- **52.2% in 1988.** Florida and Kentucky have `NA` clearances, and `violent_totals()` sums with `na.rm = TRUE`, so their crimes count as all solved.
- **41.1% in 1989 and 41.2% in 1996.** Florida reported zero clearances in both years.
- **About 3 points low in 2003–2012.** NYPD reported zero clearances, so New York's estimated rate is about 18% and stays in the pool. The site's screen covers only 2015 on.
- Other state-years with zero or missing clearances:
  - South Carolina 1991, Alabama 1993–94, Kansas 1993–99 and Iowa 1991
  - Montana 1994 and 1996, Vermont 1997, New Hampshire 1997 and Delaware 1998–2000
  - Hawaii 2002–10 (4–12%)

Rebuilding the method from agency counts lets the screen run in every year, and that removes these breaks.

### Why Crime Index rates ran higher than our first version

- The FBI's published clearance tables pool **agencies** and leave out agencies with zero clearances.
- Kaplan agency data pooled the same way matches Crime Index within about 0.5 pts in 1985–2016.
- Our first version pooled **state-years**, so a state with one big non-reporting agency (New York in 2003–12) stayed in and pulled the rate down.

### Non-reporting screen, yearly averages outside Illinois

| Decade | Agencies, ≥1 offense | Agencies, ≥10 | Agencies, ≥50 | % of crime, ≥1 | % of crime, ≥10 | % of crime, ≥50 |
|---|---|---|---|---|---|---|
| 1960s | 502 | 74 | 15 | 5.5 | 5.2 | 5.0 |
| 1980s | 905 | 121 | 36 | 1.8 | 1.7 | 1.5 |
| 2000s | 1,297 | 282 | 97 | 5.7 | 5.5 | 5.2 |
| 2020s | 1,467 | 108 | 16 | 1.4 | 1.2 | 1.0 |

- The threshold changes how many agencies are caught much more than how much crime is excluded.
- An agency with a 45% clearance rate has about a 0.3% chance of clearing none of 10 crimes by luck.
- At 10, about 160 agencies a year are caught, about 5 of them with 1,000+ offenses.
- The largest non-reporting agencies (agency names before 2000 inferred from ORI codes):
  - NYPD in 2003–12 and 2021
  - Los Angeles in 1965–68
  - Birmingham in 1965–69 and 1993–99
  - Most Florida agencies in 1989 and 1996
  - Houston and Memphis in 1982, Atlanta in 1991, Detroit in 1995

### Comparing the four methods (% of violent crime solved)

| Year | Agency-based state rates | Published method on FBI estimates | Site method rebuilt (chart) | Crime Index |
|---|---|---|---|---|
| 1965 | 59.4 | — | 60.0 | 61.1 |
| 1966 | 53.8 | — | 53.2 | 57.3 |
| 1970 | 45.6 | — | 44.9 | 47.9 |
| 1975 | 46.9 | — | 46.5 | 47.0 |
| 1980 | 43.8 | — | 43.4 | 43.6 |
| 1985 | 47.8 | 47.5 | 47.4 | 47.6 |
| 1988 | 47.0 | 52.2 | 46.8 | 45.9 |
| 1989 | 45.6 | 41.1 | 45.5 | 45.8 |
| 1996 | 47.5 | 41.2 | 46.9 | 47.9 |
| 2005 | 45.7 | 42.7 | 45.1 | 45.4 |
| 2010 | 47.5 | 44.2 | 47.1 | 47.1 |
| 2015 | 46.1 | 45.9 | 45.9 | 45.9 |
| 2020 | 42.9 | 42.9 | 42.9 | 41.7 |
| 2022 | 40.7 | 40.5 | 40.5 | 36.8 |
| 2025 | 48.0 | 47.7 | 47.7 | 47.5 |

What each method is:

- **Agency-based state rates.** Drop non-reporting agencies, give each state its own rate and apply it to that state's reported count. Illinois after 1985, and states with no clearances left, take the national rate. The script prints this as a check.
- **Published method on FBI estimates.** `us_solve_trend()`, which needs FBI-estimated clearances, so it runs from 1985 only.
- **Site method rebuilt.** The method the charts use for 1965–2014.
- **Crime Index.** FBI-table offense clearance rates times offense counts.

How they compare:

- The agency-based and rebuilt site methods agree within about 1 point in every year.
- Crime Index runs 1.5–3.5 pts higher in 1966–72. In 2022 it's 3.7 pts lower, because Crime Index has no NYPD repair.
- Agency-based state rates vs published, by state, 2015–2025:
  - The median difference is 0.08 pts, with 25 of 550 state-years off by more than 1 pt.
  - The largest is New York in 2025 (+3.0), where Nassau County PD and Syracuse PD reported no clearances.

## Open items

- Chart title and framing for publication. The current titles are placeholders.
- The navy line color (`#0B1F35`) matches the original graphic but fails the dataviz validator's lightness and chroma checks for categorical colors. Two series with direct labels and strong CVD separation make this low risk.
- Possible coverage floor, for states where non-reporting agencies hold most of the crime. Declined for now.
- Possibly moving the agency-based clearance screen into the site for all years. Not planned.
