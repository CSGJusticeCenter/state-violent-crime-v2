## State violent crime interactive pages

Sorry for the brief readme!

`R/01-` through `R/06` pull and clean various data sources needed and save to .rds to be read into qmd

`R/07-` renders each state page to html and moves html files to `_site` for deploying on Netlify

`R/08-` creates static plots for Marshall's ppt

`utils.R` contains various utility functions for the interactive pages

`state-viol-crime.qmd` is the main template file to render by state to html

`header.html` is used to create drop down menu to select state at top of each html

All data is from public sources and is contained in `data`

HTML is saved to `_site` so that it can be automatically deployed to netlify on git push

Site is published at https://csg-state-violent-crime.netlify.app/state-viol-crime-sc.html

pw: csg_crime
