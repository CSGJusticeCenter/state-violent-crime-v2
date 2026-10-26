# Justice Deficit handout: canvas edits

Notes on edits to the handout design canvas, pages 1 and 2. Written 2026-10-16.

## Canvas

- Link: <https://claude.ai/artifact/1EX1ysfVnCvQeYR2rvL18S>
- Title: "Justice Deficit Handout: Option A2 (4 pages, deficit chart)"
- Type: Design canvas, not a Claude Doc. Docs tools can't read it; the Artifact tool can.
- Pages are 816 × 1056 px (US letter). Each artboard is one file under `project/`, indexed by `project/canvas.json`.

| Artboard | File | Canvas position |
|---|---|---|
| Page 1 | `Main.dc.html` | x 0, y 0 |
| Page 2: unsolved violent crime map | `Map-unsolved.dc.html` | x 896, y 0 |
| Page 2 alt map: diverging bins | `Map-unsolved-alt.dc.html` | x 896, y -804 (above page 2) |
| Page 3 | `PageBlank.dc.html` | x 1792, y 0 |

Reference charts sit in a row at y 1500. Pages cut from the 3-page version sit at y 2700.

## Page 1

- Removed the methodology note under the two charts ("Unsolved each year is reported minus solved…").
- Removed footnote 1 at the page bottom.
- Added an empty placeholder box filling the space below the charts. It has a dashed border and gray diagonal stripes so it reads as empty.

The removed footnote was the only source line for the 4.3 million figure:

> 1. Sum of each year's reported homicides, rapes, robberies, and aggravated assaults minus those cleared, 2020–2025, FBI UCR Program.

Page 1 has no citation for that number until it goes into the box or elsewhere.

## Page 2

### Changes made

- **Map label.** Added "Share of violent crimes unsolved, 2020–2025" above the legend.
- **Legend.** The asterisk item now reads "Missing 2021 data" instead of "A year of data is missing."
- **Tile grid.** Moved out of the legend's flex container so it sits beside the legend, not inside it. Tile rows are 33 px instead of 35 px.
- **Headline.** Removed `white-space: nowrap`, so it wraps instead of running past the margin.
- **Table.** Kept all 51 rows as a state lookup and added no data columns. Changes:
  - Alternating row shading (`#eef4f9`) instead of a rule under each row
  - Type 11.5 px instead of 11 px; rows 16 px tall instead of 14 px
  - State column 118 px instead of 94 px, so "District of Columbia" fits
  - Illinois shows plain "No data" without dashed lines
- **Notes.** Cut to the caveats about this page's state data:

> Unsolved is reported minus solved, 2020–2025. States marked * or ≥ are missing 2021 data, so their shares use the years with data and their counts are minimums. New York's homicide counts also leave out 2022. Illinois has no reliable solved counts. Massachusetts, Oregon and Pennsylvania round to 50% but are just under half.

### Alternate map

The alt artboard holds only the map box, with a diverging color scale centered on 50% instead of two bins.

| Unsolved share | Fill | Text | States |
|---|---|---|---|
| Under 40% | `#0b6e5a` dark teal | white | 3 |
| 40–49% | `#c4ede0` light teal | dark | 16 |
| 50–59% | `#a9c8e8` light blue | dark | 18 |
| 60% or more | `#005DA6` blue | white | 13 |

- Teal matches the "Solved" line on page 1, and blue stands for unsolved.
- Under the color bar, "← Mostly solved / Mostly unsolved →" marks the split at half.
- Massachusetts, Oregon and Pennsylvania (shown as 50%) fall in 40–49%, matching the note.
- Illinois keeps the dashed "No data" tile.

### Suggestions declined

- Rewriting the table subhead ("States face growing backlogs…"). The table shows six-year totals, not growth.
- Adding rate or share columns to the table, or cutting it to the top states. The table stays a full lookup.
- Showing one decimal on the three tiles that round to 50%.

## Open items

- Neither page was checked in a rendered view. Page 2's larger table rows may push the footer band past the page bottom, which the root's `overflow: hidden` would clip. If so, bring rows back toward 15 px.
- The alt map frame is 460 px tall, sized by estimate. Resize it if the map is clipped or leaves a gap.
- Page 1 needs a home for the 4.3 million source line.
- Pick the binary or diverging map for page 2, then remove the other artboard.

## Editing notes

- Read each file from the canvas before editing it. Publishes are refused when someone saved a newer version of a file being sent.
- Send `canvas.json` only when adding, moving or resizing an artboard, and rebuild it from the latest published copy so hand moves on the canvas are kept.
