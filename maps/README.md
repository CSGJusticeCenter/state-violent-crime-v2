# County map data

These 51 JavaScript files are from the [Highcharts Map Collection v2.3.3](https://github.com/highcharts/map-collection-dist/releases/tag/v2.3.3), one for each state and Washington, DC. `state-viol-crime.qmd` loads them from this directory during rendering, so county map generation does not need the Highcharts CDN.

To refresh the files from the pinned release, run `Rscript R/download-county-maps.R` from the repository root. Change the version in that script deliberately when updating the map collection, then check county FIPS joins and rerun the map tests. The files retain the source copyright and attribution fields; see the [Highcharts map collection license](https://github.com/highcharts/map-collection-dist/blob/v2.3.3/LICENSE.md).
