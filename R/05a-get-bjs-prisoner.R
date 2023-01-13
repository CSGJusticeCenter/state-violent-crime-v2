## download csv files of tables from bjs prisoner series reports 2010-2011
## https://bjs.ojp.gov/library/publications/list?series_filter=Prisoners

library(tidyverse)
library(csgjcr)
library(glue)

# path to data folder on sharepoint
sp_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")
prison_path <- file.path(sp_path, "bjs_prisoners")

# function to download and unzip csvs from bjs
download_unzip_bjs_prison <- function(year) {

  file_name <- glue("p{year}")
  zip_file <- glue("p{year}.zip")

  download.file(
    url = glue("https://bjs.ojp.gov/redirect-legacy/content/pub/sheets/{file_name}.zip"),
    destfile = zip_file
  )

  unzip(
    zipfile = zip_file,
    exdir = file.path(prison_path, file_name)
  )

  unlink(zip_file)

}

# this data path works for most years, by 2012 and 2020 have different file names
walk(c(2010:2011, 2013:2019), download_unzip_bjs_prison)

# download 2012 zipped csv
download.file(
  url = "https://bjs.ojp.gov/redirect-legacy/content/pub/sheets/p12tar9112.zip",
  destfile = file.path(prison_path, glue("p12.zip"))
)

# unzip 2012 csv
unzip(
  zipfile = file.path(prison_path, glue("p12.zip")),
  exdir = file.path(prison_path, "p12")
)

# delete 2012 zipped
unlink(file.path(prison_path, glue("p12.zip")))

# download 2020 zipped csv
download.file(
  url = "https://bjs.ojp.gov/content/pub/sheets/p20st.zip",
  destfile = file.path(prison_path, glue("p20st.zip"))
)

# unzip 2020 csv
unzip(
  zipfile = file.path(prison_path, glue("p20st.zip")),
  exdir = file.path(prison_path, "p20st")
)

# delete 2020 zipped
unlink(file.path(prison_path, glue("p20st.zip")))


# 2014 files are in a weird subdirectory....why????
files_14 <- list.files(file.path(prison_path, "p14", "CSV tables"), full.names = TRUE)

# move files to the same folder as other years
file.copy(
  from = files_14,
  to = str_remove(files_14, "CSV tables/")
)

# delete 2014 csv in sub dir
unlink(file.path(prison_path, "p14", "CSV tables"), recursive = TRUE)
