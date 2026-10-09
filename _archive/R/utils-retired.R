# Retired helpers from R/utils.R. Kept for reference; nothing sources this file.

### create function to clean up and visualize SHR data
### this function will prepare a df for plotting -- grouping by
### varying incident or demographic characteristics
function_shr_grouping_for_plot <- function(df, var){

  df |>
    left_join(csg_regions_filtered,
              by = "state_abbr") |>
    ### use state name from shr df, not regions df
    ### we only use state name from regions df for filtering on correct region
    ### given a state name
    mutate(group_for_plot = case_when(state_abbr==state_abbr_params ~ csg_state_convert(state_abbr, "abbr", "name"),
                                      state_abbr!=state_abbr_params & !is.na(csg_region) ~ csg_region,
                                      TRUE ~ "drop")) |>
    filter(!.data[[var]] %in% c("Unknown", "Missing"),
           year>=2020,
           group_for_plot!="drop") |>
    dplyr::select(group_for_plot,
                  year,
                  n_total_incidents,
                  n_total_cleared,
                   all_of(var)) |>
    group_by(group_for_plot,
             .data[[var]]) |>
    ### sum across years by group
    summarize(n_total_cleared = sum(n_total_cleared, na.rm=TRUE),
              n_total_incidents = sum(n_total_incidents, na.rm=TRUE),
              clearance_rate = n_total_cleared/n_total_incidents) |>
    ungroup() |>
    # bind_rows(srs_by_cat_us) |>
    mutate(
      tooltip = paste0(
        "<b>",group_for_plot,"–",.data[[var]],"</b><br>",
        "Solve Rate: ", scales::percent(clearance_rate,
                                        accuracy = 1)),
      clearance_rate = clearance_rate*100
    )

}
