####Develop the following for Marshall
#1. Table with states with >90% NIBRS participation: # of states with increase & # of states with decrease in each type of violent crime in 2021
#2. Table with states with >90% NIBRS participation: # of states with increase in clearance & # of states with decrease in clearance for each type of violent crime in 2021
#3. Four line graphs that show violent crime rates over time by crime (the current viz is a stacked line chart - he wants them separately) for states with >90% NIBRS participation

library(tidyverse)
library(scales)
library(csgjcr)
library(highcharter)
library(htmlwidgets)
library(reactable)
library(openxlsx)

source("utils.R")

##PREP
# path to data folder on sharepoint
sp_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")

# read in crime data
viol_crime_us                <- read_rds(file.path(sp_path, "viol_crime_us.rds"))
viol_crime_by_off_us         <- read_rds(file.path(sp_path, "viol_crime_by_off_us.rds"))
viol_crime_by_state          <- read_rds(file.path(sp_path, "viol_crime_by_state.rds"))
viol_crime_by_off_state      <- read_rds(file.path(sp_path, "viol_crime_by_off_state.rds"))
viol_crime_2021_state        <- read_rds(file.path(sp_path, "viol_crime_2021_state.rds"))
viol_crime_2021_by_off_state <- read_rds(file.path(sp_path, "viol_crime_2021_by_off_state.rds"))
viol_crime_2021_by_off       <- read_rds(file.path(sp_path, "viol_crime_2021_by_off.rds"))
ucr_agency_pop               <- read_rds(file.path(sp_path, "ucr_agency_pop.rds"))
state_nibrs_pop_cov          <- read_rds(file.path(sp_path, "state_nibrs_pop_cov.rds"))

# read in prison population data
us_prison                    <- read_rds(file.path(sp_path, "us_prison.rds"))
viol_prison_pop_state        <- read_rds(file.path(sp_path, "viol_prison_pop_state.rds"))
viol_prison_pop_by_off_state <- read_rds(file.path(sp_path, "viol_prison_pop_by_off_state.rds"))

offense_pal <- tibble(
  color = jr_pal[1:4],
  crime = c("Homicide", "Robbery", "Rape", "Aggravated assault")
)

#states w/ >=90% NIBRS participation/coverage, create as vector for reference later
states.gte90 <- state_nibrs_pop_cov |>
  filter(pop_cov>=0.9) |>
  pull(state_abb)

##########RATE
#break out states BY OFFENSE, USA (2019 and 2020)
table_vcr.all <- viol_crime_by_off_us |>
  left_join(offense_pal, by = "crime") |>
  filter(year %in% c(2019,2020)) |>
  group_by(year,crime) |>
  summarise(across(c(actual_est,pop_total), list(sum))) |>
  group_by(year) |>
  mutate(
    rate = actual_est_1 / pop_total_1 * 1e4 #want the rate at 10K
  )

#break out states BY OFFENSE, only states where NIBRS participation >=90% (2021)
table_vcr.gte90_21 <- viol_crime_2021_by_off_state |>
    left_join(offense_pal, by = "crime") |>
    filter(state_abb %in% states.gte90) |>
    group_by(year,crime) |>
    summarise(across(c(actual_est,pop_total), list(sum))) |>
    mutate(
      rate = actual_est_1/pop_total_1 * 1e4 #want the rate at 10K
    )

table_vcr.gte90_20 <- viol_crime_by_off_state |>
  left_join(offense_pal, by = "crime") |>
  filter(year == 2020 & state_abb %in% states.gte90) |>
  group_by(year,crime) |>
  summarise(across(c(actual_est,pop_total), list(sum))) |>
  group_by(year) |>
  mutate(
    rate = actual_est_1 / pop_total_1 * 1e4 #want the rate at 10K
  )

table_vcr.gte90_19 <- viol_crime_by_off_state |>
  left_join(offense_pal, by = "crime") |>
  filter(year == 2019 & state_abb %in% states.gte90) |>
  group_by(year,crime) |>
  summarise(across(c(actual_est,pop_total), list(sum))) |>
  group_by(year) |>
  mutate(
    rate = actual_est_1 / pop_total_1 * 1e4 #want the rate at 10K
  )

#combine years, count #states increase/decrease in violent crime BY OFFENSE
table_vcr.gte90_21_ct <- viol_crime_2021_by_off_state |>
  left_join(offense_pal, by = "crime") |>
  filter(state_abb %in% states.gte90) |>
  select(state_abb, crime, actual_est) |>
  rename(actual_est21 = actual_est)

table_vcr.gte90_19_ct <- viol_crime_by_off_state |>
  left_join(offense_pal, by = "crime") |>
  filter(year == 2019 & state_abb %in% states.gte90) |>
  select(state_abb, crime, actual_est) |>
  rename(actual_est19 = actual_est)

table_ct <- merge(table_vcr.gte90_19_ct,table_vcr.gte90_21_ct, by = c("state_abb", "crime")) |>
  mutate(
    decrease = ifelse(actual_est19-actual_est21>0,1,0),
    increase = ifelse(actual_est19-actual_est21<0,1,0)
  ) |>
  group_by(crime) |>
  summarise(across(c(decrease,increase),list(sum))) |>
  rename(decrease19_21 = decrease_1,
         increase19_21 = increase_1)

#combine years, count #states increase/decrease in violent crime OVERALL
table_vcr.gte90_21_ctov <- viol_crime_2021_state |>
  filter(state_abb %in% states.gte90) |>
  select(state_abb, actual_est) |>
  rename(actual_est21 = actual_est)

table_vcr.gte90_19_ctov <- viol_crime_by_state |>
  filter(year == 2019 & state_abb %in% states.gte90) |>
  select(state_abb, actual_est) |>
  rename(actual_est19 = actual_est)

table_ctov <- merge(table_vcr.gte90_19_ctov,table_vcr.gte90_21_ctov, by = "state_abb") |>
  mutate(
    decrease = ifelse(actual_est19-actual_est21>0,1,0),
    increase = ifelse(actual_est19-actual_est21<0,1,0)
  ) |>
  summarise(across(c(decrease,increase),list(sum))) |>
  rename(decrease19_21 = decrease_1,
         increase19_21 = increase_1)

#final table
change <- merge(table_vcr.gte90_19,
                table_vcr.gte90_20,
                by = "crime") |>
  left_join(table_vcr.gte90_21, by = "crime") |>
  mutate(change_rate19_21 = rate.x - rate,
         change_rate20_21 = rate.y - rate) |>
  rename(
    rate2019 = rate.x,
    rate2020 = rate.y,
    rate2021 = rate,
  ) |>
  select(crime, rate2019, rate2020, rate2021, change_rate20_21, change_rate19_21) |>
  left_join(table_ct, by = "crime")


##########CLEARANCE
#break out states BY OFFENSE, USA (2019 and 2020)
table_vcr.all <- viol_crime_by_off_us |>
  left_join(offense_pal, by = "crime") |>
  filter(year %in% c(2019,2020)) |>
  group_by(year,crime) |>
  summarise(across(c(cleared,actual), list(sum))) |>
  group_by(year) |>
  mutate(
    clearance_rate = cleared_1 / actual_1
  )

#break out states BY OFFENSE, only states where NIBRS participation >=90% (2021)
table_vcr.gte90_21 <- viol_crime_2021_by_off_state |>
  left_join(offense_pal, by = "crime") |>
  filter(state_abb %in% states.gte90) |>
  group_by(year,crime) |>
  summarise(across(c(cleared,actual), list(sum))) |>
  mutate(
    clearance_rate = cleared_1 / actual_1
  )

table_vcr.gte90_20 <- viol_crime_by_off_state |>
  left_join(offense_pal, by = "crime") |>
  filter(year == 2020 & state_abb %in% states.gte90) |>
  group_by(year,crime) |>
  summarise(across(c(cleared,actual), list(sum))) |>
  group_by(year) |>
  mutate(
    clearance_rate = cleared_1 / actual_1
  )

table_vcr.gte90_19 <- viol_crime_by_off_state |>
  left_join(offense_pal, by = "crime") |>
  filter(year == 2019 & state_abb %in% states.gte90) |>
  group_by(year,crime) |>
  summarise(across(c(cleared,actual), list(sum))) |>
  group_by(year) |>
  mutate(
    clearance_rate = cleared_1 / actual_1
  )

#combine years, count #states increase/decrease in violent crime BY OFFENSE
table_vcr.gte90_21_ct <- viol_crime_2021_by_off_state |>
  left_join(offense_pal, by = "crime") |>
  filter(state_abb %in% states.gte90) |>
  select(state_abb, crime, clearance_rate) |>
  rename(clearance_rate21 = clearance_rate)

table_vcr.gte90_19_ct <- viol_crime_by_off_state |>
  left_join(offense_pal, by = "crime") |>
  filter(year == 2019 & state_abb %in% states.gte90) |>
  select(state_abb, crime, clearance_rate) |>
  rename(clearance_rate19 = clearance_rate)

table_ct <- merge(table_vcr.gte90_19_ct,table_vcr.gte90_21_ct, by = c("state_abb", "crime")) |>
  mutate(
    decrease = ifelse(clearance_rate19-clearance_rate21>0,1,0),
    increase = ifelse(clearance_rate19-clearance_rate21<0,1,0)
  ) |>
  group_by(crime) |>
  summarise(across(c(decrease,increase),list(sum))) |>
  rename(decrease19_21 = decrease_1,
         increase19_21 = increase_1)

#final table
change.clear <- merge(table_vcr.gte90_19,
                      table_vcr.gte90_20,
                      by = "crime") |>
  left_join(table_vcr.gte90_21, by = "crime") |>
  mutate(change_rate19_21 = clearance_rate.x - clearance_rate,
         change_rate20_21 = clearance_rate.y - clearance_rate) |>
  rename(
    rate2019 = clearance_rate.x,
    rate2020 = clearance_rate.y,
    rate2021 = clearance_rate,
  ) |>
  select(crime, rate2019, rate2020, rate2021, change_rate19_21, change_rate20_21) |>
  left_join(table_ct, by = "crime")


write.xlsx(change,       "rates.csv",           sheetName="Sheet1")
write.xlsx(change.clear, "clearance_rates.csv", sheetName="Sheet1")

#DEVELOP WEIGHTS based on state population size?
#
# weights <- viol_crime_2021_state |>
#   filter(state_abb %in% states.gte90) |>
#   mutate(
#     gte90_tot = sum(pop_total),
#     weight    = pop_total/gte90_tot
#   ) |>
#   select(state_abb,weight)
#
#
# table_vcr.gte90 <- viol_crime_2021_by_off_state |>
#   left_join(offense_pal, by = "crime") |>
#   left_join(weights,     by = "state_abb") |>
#   filter(state_abb %in% states.gte90) |>
#   group_by(year,crime) |>
#   mutate(
#     weighted_est = actual_est * weight
#   ) |>
#   summarise(across(c(weighted_est,pop_total), list(sum))) |>
#   mutate(
#     rate = (weighted_est_1/pop_total_1 *1e4)*length(states.gte90) #want the rate at 10K
#   )

#create violent crime rate plots by offense per 10k pop.
gte90_21 <- viol_crime_2021_by_off_state |>
  left_join(offense_pal, by = "crime") |>
  filter(state_abb %in% states.gte90) |>
  group_by(year,crime) |>
  summarise(across(c(actual_est,pop_total), list(sum))) |>
  mutate(
    rate = actual_est_1 / pop_total_1 * 1e4 #want the rate at 10K
  )

gte90_10_20 <- viol_crime_by_off_state |>
  left_join(offense_pal, by = "crime") |>
  filter(state_abb %in% states.gte90) |>
  group_by(year,crime) |>
  summarise(across(c(actual_est,pop_total), list(sum))) |>
  mutate(
    rate = actual_est_1 / pop_total_1 * 1e4 #want the rate at 10K
  )

viol_crime_by_off_us_sep <- gte90_10_20 |>
  bind_rows(gte90_21)

to_plot <- viol_crime_by_off_us_sep |>
  left_join(offense_pal, by = "crime") |>
  mutate(
    crime = fct_reorder(crime, rate, last)
  ) |>
  arrange(desc(year), rate)

plot1<- to_plot[which(to_plot$crime=="Aggravated assault"),] |>
  hchart(
    "spline",
    hcaes(year, rate, group = crime),
    color = "#E17619"
  ) |>
  hc_colors(unique(to_plot$color)) |>
  hc_title(text = paste0("Violent crime rate in the US per 10k residents: Aggravated assault")) |>
  hc_caption(text = paste0("FBI Uniform Crime Reporting Program<br>Aggregated from SRS agency-level Offenses Known reports")) |>
  hc_setup() |>
  hc_exporting(enabled = FALSE) |>
  hc_tooltip(enabled=FALSE)
plot2<- to_plot[which(to_plot$crime=="Rape"),] |>
  hchart(
    "spline",
    hcaes(year, rate, group = crime),
    color = "#50A25D"
  ) |>
  hc_colors(unique(to_plot$color)) |>
  hc_title(text = paste0("Violent crime rate in the US per 10k residents: Rape")) |>
  hc_caption(text = paste0("FBI Uniform Crime Reporting Program<br>Aggregated from SRS agency-level Offenses Known reports")) |>
  hc_setup() |>
  hc_exporting(enabled = FALSE) |>
  hc_tooltip(enabled=FALSE) |>
  hc_yAxis(labels = list(format = "{value:,.1f}"))
plot3<- to_plot[which(to_plot$crime=="Robbery"),] |>
  hchart(
    "spline",
    hcaes(year, rate, group = crime),
    color = "#273C4C"
  ) |>
  hc_colors(unique(to_plot$color)) |>
  hc_title(text = paste0("Violent crime rate in the US per 10k residents: Robbery")) |>
  hc_caption(text = paste0("FBI Uniform Crime Reporting Program<br>Aggregated from SRS agency-level Offenses Known reports")) |>
  hc_setup() |>
  hc_exporting(enabled = FALSE) |>
  hc_tooltip(enabled=FALSE) |>
  hc_yAxis(labels = list(format = "{value:,.1f}"))
plot4<- to_plot[which(to_plot$crime=="Homicide"),] |>
  hchart(
    "spline",
    hcaes(year, rate, group = crime),
    color = "#4095B1",
  ) |>
  hc_title(text = paste0("Violent crime rate in the US per 10k residents: Homicide")) |>
  hc_caption(text = paste0("FBI Uniform Crime Reporting Program<br>Aggregated from SRS agency-level Offenses Known reports")) |>
  hc_setup() |>
  hc_exporting(enabled = FALSE) |>
  hc_tooltip(enabled=FALSE) |>
  hc_yAxis(labels = list(format = "{value:,.2f}"))

#grid of plots
finalgrid <- hw_grid(plot1,plot2,plot3,plot4,ncol=2)
