## functions and settings for highcharter plots

# set options to use comma separator for 1000s
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# set fonts to use as defaults
default_fonts <- c("GT-America", "sans-serif")
header_font <- c("GT-America", "sans-serif")
header_weight <- 700

# define justice reinvestment color palette
jr_pal <- c("#4095B1", "#273C4C", "#50A25D", "#E17619", "#E25449", "#779F38", "#AFABAB")

# define theme for highcharter
hc_theme_jc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    colors = jr_pal,
    chart = list(
      style = list(fontFamily = default_fonts)
    ),
    title = list(style = list(fontFamily = header_font, color = "#005FAD",
                              fontSize = "24px")),
    subtitle = list(style = list(fontFamily = default_fonts, fontSize = "16px",
                                 color = "#666666")),
    legend = list(align = "center", verticalAlign = "bottom"),
    caption = list(align = "right"),
    plotOptions = list(
      series = list(states = list(inactive = list(opacity = 1))),
      line = list(marker = list(enabled = TRUE)),
      spline = list(marker = list(enabled = TRUE)),
      area = list(marker = list(enabled = TRUE)),
      areaspline = list(marker = list(enabled = TRUE))
    )
  )
)

render_image <- JS("
  function(){
    this.renderer.image('https://csg-state-violent-crime.netlify.app/img/csgjc-logo.png', 30, this.chartHeight - 37, 140.1, 30)
    .add();
  }")

render_image_print <- JS("
  function(){
    logo=this.renderer.image('https://csg-state-violent-crime.netlify.app/img/csgjc-logo.png', 30, this.chartHeight - 37, 140.1, 30)
    .add(); this.print();
  }")

render_image_remove <- JS("function(){logo.element.remove();}")

# define default setup for highcharter plots
# add and configure exporting and accessibility modules
# set justice center theme
# set default tooltip text to be in input data column `tooltip`
hc_setup <- function(x) {
  hc_add_dependency(x, name = "modules/exporting.js") |>
    hc_add_dependency(name = "modules/offline-exporting.js") |>
    hc_add_dependency(name = "modules/accessibility.js") |>
    hc_exporting(
      enabled = TRUE,
      buttons = list(contextButton = list(menuItems = list("downloadPNG", "printChart"))),
      accessibility = list(enabled = TRUE)
    ) |>
    hc_add_theme(hc_theme_jc) |>
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |>
    hc_plotOptions(
      series = list(animation = FALSE),
      accessibility = list(
        enabled = TRUE,
        keyboardNavigation = list(enabled = TRUE)
      )
    ) |>
    hc_xAxis(
      title = "",
      labels = list(y = 25)
    ) |>
    hc_yAxis(
      title = "",
      labels = list(format = "{value:,.0f}")
    ) |>
  hc_exporting(
    chartOptions = list(
      chart = list(
        events = list(
          load = render_image
        )
      )
    )
  ) |>
  hc_chart(
    events = list(
      beforePrint = render_image_print,
      afterPrint = render_image_remove
    )
  )
}

offense_pal <- tibble(
  color = jr_pal[c(2,3,4,5)],
  crime = c("Homicide", "Robbery", "Rape", "Aggravated assault")
)

reactable_template <- function(df, sort_col = "rate", ...) {
  reactable(
    df,
    highlight = TRUE,
    searchable = TRUE,
    defaultSorted = sort_col,
    showPageSizeOptions	= TRUE,
    pageSizeOptions = c(10, 25, 100),
    defaultColDef = colDef(
      vAlign = "center",
      format = colFormat(digits = 0, separators = TRUE),
      headerStyle = list(fontWeight = 700, fontFamily = default_fonts,
                         fontVariant = "all-petite-caps"),
      style = list(fontWeight = 400, fontFamily = "Source Sans Pro")
    ),
    ...
  )
}

### function for percent change
add_plus_sign_percent_change <- function(value) {

  if (!is.numeric(value))
    paste0(value)

  if (!is.na(value) & value >= 0)
    paste0("+", round(value,digits = 0),"%")

  else
    paste0(round(value,digits=0),"%")

}

## function for percent point change
add_plus_sign_percent_point_change <- function(value) {

  if (!is.numeric(value))
    paste0(value)

  if (!is.na(value) & value >= 0)
    paste0("+", round(value,digits = 0))

  else
    paste0(round(value,digits=0))
}

### create not in function to use below via negate
`%nin%` <- Negate(`%in%`)

### create function to clean up and visualize SHR data
### this function will prepare a df for plotting -- grouping by
### varying incident or demographic characteristics
function_shr_grouping_for_national_plot <- function(df, var){

  df |>
    filter(!!sym(var)%nin%c("Unknown","Missing"),
           year>=2020) |>
    dplyr::select(year,
                  n_total_incidents,
                  n_total_cleared,
                  !!sym(var)) |>
    group_by(!!sym(var)) |>
    ### sum across years by group
    summarize(n_total_cleared = sum(n_total_cleared, na.rm=TRUE),
              n_total_incidents = sum(n_total_incidents, na.rm=TRUE),
              clearance_rate = n_total_cleared/n_total_incidents) |>
    ungroup() |>
    # bind_rows(srs_by_cat_us) |>
    mutate(
      tooltip = paste0(
        "<b>","United States","–",!!sym(var),"</b><br>",
        "Solve Rate: ", scales::percent(clearance_rate,
                                        accuracy = 1)),
      clearance_rate = clearance_rate*100
    )

}


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
    mutate(group_for_plot = case_when(state_abbr==state_abbr_params ~ state_name.x,
                                      state_abbr!=state_abbr_params & !is.na(csg_region) ~ csg_region,
                                      TRUE ~ "drop")) |>
    filter(!!sym(var)%nin%c("Unknown","Missing"),
           year>=2018,
           group_for_plot!="drop") |>
    dplyr::select(group_for_plot,
                  year,
                  n_total_incidents,
                  n_total_cleared,
                  !!sym(var)) |>
    group_by(group_for_plot,
             !!sym(var)) |>
    ### sum across years by group
    summarize(n_total_cleared = sum(n_total_cleared, na.rm=TRUE),
              n_total_incidents = sum(n_total_incidents, na.rm=TRUE),
              clearance_rate = n_total_cleared/n_total_incidents) |>
    ungroup() |>
    # bind_rows(srs_by_cat_us) |>
    mutate(
      tooltip = paste0(
        "<b>",group_for_plot,"–",!!sym(var),"</b><br>",
        "Solve Rate: ", scales::percent(clearance_rate,
                                        accuracy = 1)),
      clearance_rate = clearance_rate*100
    )

}
