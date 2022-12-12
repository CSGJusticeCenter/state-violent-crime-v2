## functions and settings for highcharter plots

# set options to use comma separator for 1000s
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# set highcharter fonts to match Quarto default fonts
default_fonts <- "GT America Regular"
header_font <- "GT America"
header_weight <- 700

# define justice reinvestment color palette
jr_pal <- c("#4095B1", "#273C4C", "#50A25D", "#E17619", "#E25449", "#779F38", "#AFABAB")

# define theme for highcharter
hc_theme_jc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    colors = jr_pal,
    chart = list(
      marginTop = 75,
      style = list(fontFamily = default_fonts)
    ),
    title = list(style = list(fontFamily = header_font, color = "#004270",
                              fontSize = "24px", fontWeight = 700)),
    subtitle = list(style = list(fontFamily = default_fonts, fontSize = "16px")),
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

# define default setup for highcharter plots
# add and configure exporting and accessibility modules
# set justice center theme
# set default tooltip text to be in input data column `tooltip`
hc_setup <- function(x) {
  hc_add_dependency(x, name = "modules/exporting.js") %>%
    hc_add_dependency(name = "modules/offline-exporting.js") %>%
    hc_add_dependency(name = "modules/accessibility.js") %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(contextButton = list(menuItems = list("printChart", "downloadPNG"))),
      accessibility = list(enabled = TRUE)
    ) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_plotOptions(
      series = list(animation = FALSE),
      accessibility = list(
        enabled = TRUE,
        keyboardNavigation = list(enabled = TRUE)
      )
    ) %>%
    hc_xAxis(
      title = "",
      labels = list(y = 25)
    ) %>%
    hc_yAxis(
      title = "",
      labels = list(format = "{value:,.0f}")
    )
}
