# Illinois solve rates

Branch `illinois-solve-rates`, October 2026. Parked, not merged. `main` and the live site still exclude Illinois from all solve rates.

## Status

- The code on this branch works. Tests pass, and the IL, NY and U.S. pages render.
- It's parked because Chicago PD's NIBRS data looks incomplete. See [Chicago data problems](#chicago-data-problems).
- Before merging, decide how to handle Chicago homicide and aggravated assault. See [Options](#options).

## What the branch changes

| Area | Change |
|---|---|
| `R/page-data.R` | `solve_rate_first_years <- c(Illinois = 2022)` replaces `states_to_exclude_from_solved_rate_viz` |
| Hiding | Years before a state's first solve year set `hide_total_solve` and `hide_offense_solve`, so the hide flags drive charts, maps, U.S. pools and exports |
| U.S. solve rates | Include Illinois from 2022 on. Lowers the U.S. violent crime rate by about 0.5 points from 2022 on |
| Illinois page | Trend charts start in 2022. Narrative compares 2025 with 2022. County map and agency table are shown, with the change since 2019 blank |
| State page | `show-solved` switch and "did not report enough data" placeholders removed, since every state now has solve data |
| National page | Illinois has a 2025 solve rate on the map and in the offense table. Gray on the change map, with a tooltip and a sentence saying it lacks 2019 data |
| Methodology | `illinois_solve_note()` explains the gap on every state page and the national page |
| `exclude` arguments | Removed from `solve_rate_pool()`, `us_solve_trend()`, `us_solve_benchmark()` and `solve_rate_hidden()`. Tests use hide flags instead |

## Why 2022

- Illinois' state UCR program never collected clearances from summary agencies. Clearances only reached the FBI once agencies moved to NIBRS.
- In `data/clearance_screen.rds`, agencies reporting no clearances hold 93–95% of Illinois offenses in 2015–2020 and under 1.1% in 2021–2025.
- 2021 coverage was 54% (`data/ucr_coverage_2021.csv`), below the 70% bar, so the existing rule already hides 2021.
- 2025 coverage is 95.5% (`data/state_coverage.rds`).

## Illinois NIBRS portal vs FBI

The Group A Offense Report exports from [ilucr.nibrs.com](https://ilucr.nibrs.com/ReportsIndex/List) (2020–2025) hold the same NIBRS submissions the FBI gets. They add nothing for 2019 or 2020, so they weren't kept.

Each cell shows the portal count as a share of the FBI estimate, then portal vs FBI solve rate.

| Year | Homicide | Rape | Robbery | Agg. assault |
|---|---|---|---|---|
| 2021 | 51% · 26 vs 24% | 44% · 14 vs 11% | 56% · 11 vs 10% | 41% · 33 vs 32% |
| 2022 | 91% · 33 vs 31% | 75% · 12 vs 11% | 106% · 10 vs 10% | 86% · 35 vs 35% |
| 2025 | 100% · 40 vs 44% | 98% · 11 vs 13% | 100% · 18 vs 18% | 98% · 44 vs 44% |

## Chicago data problems

Chicago PD (`ILCPD0000`) reports all 12 months and sends clearances from 2022 on. It accounts for 37% of Illinois' reported violent crime in 2025. The comparison uses the city's [Crimes 2001–present](https://data.cityofchicago.org/resource/ijzp-q8t2.json) data, which has an arrest flag but no clearance field. Script: `_notes/chicago-vs-fbi.R`.

### Homicide clearances missing

| Year | City homicides | FBI homicides | City arrest share | FBI clearance rate | CPD/OIG clearance rate |
|---|---|---|---|---|---|
| 2022 | 731 | 604 | 44% | 20% | — |
| 2024 | 593 | 461 | 40% | 17% | 55–56% |
| 2025 | 435 | 346 | 42% | 28% | 71% |

- CPD reports 296 homicide clearances in 2025, while the FBI file has 96. Both count clearances in the year they happen and include exceptional clearances.
- The FBI file has 15–20% fewer homicides than city data each year since 2022.
- CPD's rate is disputed. An independent FOIA analysis puts 2025 near 68%, and the mayor's office has claimed 77%.

### Aggravated assault cases missing

| Year | City agg. assault + battery (04A, 04B) | FBI agg. assault | FBI share |
|---|---|---|---|
| 2019 (SRS) | 13,708 | 15,283 | 111% |
| 2022 | 14,789 | 3,395 | 23% |
| 2025 | 13,714 | 3,748 | 27% |

- The count drops about 75% at the NIBRS switch while the city count stays flat. Chicago probably sends many aggravated batteries as simple assaults. Unconfirmed.
- This drives most of the drop in the FBI's Illinois aggravated assault estimate, from 32,929 (2019) to 18,674 (2022). It affects Illinois crime counts on `main` too, not just solve rates.
- The FBI clearance rate (29% in 2025) sits near the city's arrest share (20%), but it covers an unknown subset of cases.

### Robbery and rape

- Robbery counts match within 2% and rates are close (2025: city arrest share 10.2%, FBI clearance rate 14.1%).
- Rape rates are close, but the FBI file is missing 15–30% of the city's cases.

## Chicago vs rest of Illinois, 2025 (FBI)

| Offense | Chicago PD | Rest of Illinois |
|---|---|---|
| Homicide | 28% | 71% |
| Rape | 8% | 15% |
| Robbery | 14% | 29% |
| Aggravated assault | 29% | 48% |
| Violent crime | 19% | 40% |

## Options

1. **Keep Illinois excluded** (`main` today).
2. **Merge this branch and hide Illinois homicide** through `hide_offense_solve`, as with New York 2022. Aggravated assault stays shown with a caveat.
3. **Repair Chicago homicide** with CPD or OIG clearance rates, as with the NYPD 2022 repair. Harder to defend, since CPD's figure is disputed and covers only 2024–2025.
4. **Ask the source.** The Illinois State Police or CPD could explain how CPD maps aggravated battery and clearances into NIBRS.

## Sources

- [Crime in Illinois 2019, Introduction](https://web-akamai.isp.illinois.gov/StaticFiles/docs/CrimeReporting/cii/cii19/CII_19_Introduction.pdf), pp. 3 and 10, on I-UCR collecting offenses and arrests but not clearances
- [Fox 32 on CPD's 2025 homicide clearance rate](https://www.fox32chicago.com/news/chicago-homicides-fall-2025)
- [Chicago Tribune via Wirepoints on 2025 clearances](https://wirepoints.org/one-detectives-work-demonstrates-how-cpd-improved-the-homicide-clearance-rate-chicago-tribune-yahoo/)
- [Hyde Park Herald on the OIG's 2025 rate](https://www.hpherald.com/evening_digest/police-homicide-clearance-rate-hits-13-year-high-as-murders-fall-dramatically-citywide/article_74ff6015-4da0-4584-944b-354034b61696.html)
- [City That Works on exceptional clearances](https://citythatworks.substack.com/p/getting-away-with-murder)
