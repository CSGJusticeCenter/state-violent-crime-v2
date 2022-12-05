## using ncrp data, calculate prison population for violent and non violent offenses
## for violent offenses also count offense detail by year and state

library(tidyverse)

# path to data folder on sharepoint
sp_path <- csg_sp_path("ad_hoc_requests/state_violent_crime_marshall/data")

# load ncrp data, ds0004 (year-end population)
# https://www.icpsr.umich.edu/web/NACJD/studies/38048
load(file.path(sp_path, "ICPSR_38048-V1/ICPSR_38048/DS0004/38048-0004-Data.rda"))

# only 2010 and more recent and sentences greater than a year
# TODO: check with jess if correct to limit to 1+ year sentences
ncrp <- da38048.0004 |>
  as_tibble() |>
  filter(RPTYEAR >= 2010, !SENTLGTH %in% "(0) < 1 year") |>
  mutate(
    year = RPTYEAR,
    violent_off = if_else(OFFGENERAL %in% "(1) Violent", "Violent", "Nonviolent")
  )

# count violent and nonviolent prison pop by year by state
# clean up state column
viol_prison_pop_state <- ncrp |>
  count(STATE, year, violent_off) |>
  separate(STATE, into = c("a", "state"), sep = " ", extra = "merge") |>
  select(-a)

# count violent offenses by offense detail prison pop by year by state
# combine negligent manslaughter and other violent offenses to better match UCR homicide definition
# TODO: check with jess if correct
# clean up state column
viol_prison_pop_by_off_state <- ncrp |>
  filter(violent_off == "Violent") |>
  count(STATE, year, OFFDETAIL) |>
  mutate(
    off_comb = case_when(
      str_detect(OFFDETAIL, "Murder") ~ "Homicide",
      str_detect(OFFDETAIL, "Rape")                ~ "Rape",
      str_detect(OFFDETAIL, "Robbery")             ~ "Robbery",
      str_detect(OFFDETAIL, "Aggravated")          ~ "Aggravated or simple assault",
      str_detect(OFFDETAIL, "Other|manslaughter")  ~ "Other violent offenses"
      )
    ) |>
  separate(STATE, into = c("a", "state"), sep = " ", extra = "merge") |>
  select(-a, -OFFDETAIL)

# write prison pop to disk
write_rds(viol_prison_pop_state, file.path(sp_path, "viol_prison_pop_state.rds"))
write_rds(viol_prison_pop_by_off_state, file.paht(sp_path, "viol_prison_pop_by_off_state.rds"))



# not use currently - perhaps in future calculate prison sentence at admission
# load("data/ICPSR_38048-V1/ICPSR_38048/DS0002/38048-0002-Data.rda")
#
# df <- da38048.0002 |>
#   as_tibble() |>
#   filter(
#     RPTYEAR >= 2010,
#     ADMTYPE == "(1) New court commitment",
#     OFFGENERAL == "(1) Violent",
#     !is.na(SENTLGTH)
#     ) |>
#   mutate(
#     off_comb = case_when(
#       str_detect(OFFDETAIL, "Murder|manslaughter") ~ "Homicide",
#       str_detect(OFFDETAIL, "Rape")       ~ "Rape",
#       str_detect(OFFDETAIL, "Robbery")    ~ "Robbery",
#       str_detect(OFFDETAIL, "Aggravated") ~ "Aggravated or simple assault",
#       str_detect(OFFDETAIL, "Other")      ~ "Other violent offenses"
#     ),
#     sent_short = case_when(
#       str_detect(SENTLGTH, "< 1 year|1-1.9 years") ~ "Fewer than 2 years",
#       str_detect(SENTLGTH, "2-4.9 years") ~ "2 to 5 years",
#       str_detect(SENTLGTH, "5-9.9 years") ~ "5 to 10 years",
#       str_detect(SENTLGTH, "10-24.9 years") ~ "10 to 25 years",
#       TRUE ~ "25 years or more"
#     ),
#     sent_short = fct_relevel(sent_short, "Fewer than 2 years", "2 to 5 years",
#                              "5 to 10 years", "10 to 25 years", "25 years or more"),
#     sent_short = fct_rev(sent_short)
#   )
#
# df |>
#   filter(off_comb != "Other violent offenses") |>
#   count(RPTYEAR, off_comb, sent_short) |>
#   group_by(RPTYEAR, off_comb) |>
#   mutate(pct = n / sum(n)) |>
#   ungroup() |>
#   mutate(off_comb = fct_relevel(off_comb, "Homicide", "Rape", "Robbery")) |>
#   ggplot(aes(RPTYEAR, pct, color = sent_short)) +
#   geom_line(lwd = 1, alpha = 0.8) +
#   geom_point(size = 2) +
#   scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
#   scale_y_continuous(
#     labels = label_percent()
#   ) +
#   scale_color_csg() +
#   facet_wrap(vars(off_comb)) +
#   labs(
#     title = "Sentence length for violent offenses at admission to state prison",
#     x = NULL,
#     y = NULL,
#     color = NULL
#     )
#

