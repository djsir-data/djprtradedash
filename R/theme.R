# Highchart theme
djpr_highcharts <- function(hc){

  highcharter::hc_add_theme(
    hc,
    highcharter::hc_theme(
      chart = list(
        backgroundColor = NULL,
        style = list(
          fontFamily = "VIC-Regular",
          `font-size` = "14px",
          color = "#000000"
        )
      ),
      title = list(
        align = "left",
        # x = 75,
        style = list(
          color = "#000000",
          fontFamily = "VIC-Regular",
          `font-weight` =  "bold",
          `font-size` = "22px",
          `background-color` = "#000000"
        )
      ),
      subtitle = list(
        align = "left",
        # x = 75,
        style = list(
          color = "#000000",
          fontFamily = "VIC-Regular",
          `font-size` = "16px"
        )
      ),
      xAxis = list(
        labels = list(
          style = list(
            color = "#000000",
            `font-weight` =  "bold",
            `font-size` = "12px"
          )
        ),
        title = list(style = list(color = "#000000", `font-weight` =  "bold")),
        lineWidth = 3,
        lineColor = "#000000",
        tickPosition = "inside",
        tickColor = "#000000",
        tickWidth = 3,
        tickLength = 7
      ),
      yAxis = list(
        labels = list(
          style = list(
            color = "#000000",
            `font-weight` =  "bold",
            `font-size` = "12px"
          )
        ),
        title = list(style = list(color = "#000000", `font-weight` =  "bold")),
        minorGridLineWidth = 0,
        lineWidth = 3,
        lineColor = "#000000",
        gridLineColor = "transparent",
        opposite = FALSE
      ),
      navigator = list(
        enabled = FALSE
      ),
      scrollbar = list(
        enabled = FALSE
      ),
      caption = list(
        # x = 75,
        style = list(
          color = "#000000"
        )
      ),
      rangeSelector = list(
        labelStyle = list(color = "#000000"),
        floating = TRUE,
        buttonPosition = list(
          align = "right",
          x = -10,
          y = 10
        )
      ),
      series = list(
        line = list(lineWidth = 4),
        spline = list(lineWidth = 4)
      ),
      exporting = list(enabled = TRUE)
    )
  )

}


# Generic table generation fun
djpr_table <- function(df, first_col_header = TRUE){
  # Table container
  shiny::tags$table(
    class = "djprTable",
    # Header row
    shiny::tags$thead(
      shiny::tags$tr(lapply(colnames(df), function(x) shiny::tags$th(scope = "col", x)))
    ),
    # Table body
    shiny::tags$tbody(
      apply(df, 1, function(x) {
        shiny::tags$tr(
          c(
            list(
              if(first_col_header) {
                shiny::tags$th(scope = "row", x[[1]])
              } else {
                shiny::tags$td(x[[1]])
              }
            ),
            lapply(x[2:length(x)], function(y) shiny::tags$td(y))
          )
        )
      }
      )
    )
  )
}


# Format dollar figures
dollar_stat <- function(stat){
  dplyr::case_when(
    stat > 1e10 ~ scales::dollar(
      stat / 1e09,
      suffix = "b"
    ),
    stat > 1e09 ~ scales::dollar(
      stat / 1e09,
      suffix = "b"
    ),
    stat > 1e07 ~ scales::dollar(
      stat / 1e06,
      prefix = "$",
      suffix = "m"
    ),
    stat > 1e06 ~ scales::dollar(
      stat / 1e06,
      prefix = "$",
      suffix = "m"
    ),
    stat > 1e04 ~ scales::dollar(
      stat / 1e03,
      suffix = "k"
    ),
    stat > 1e03 ~ scales::dollar(
      stat / 1e03,
      suffix = "k"
    ),
    TRUE ~ scales::dollar(
      stat,
      accuracy = 1
    )
  )
}

format_table_num <- function(x, suffix = "m", round = 0, plus_neg = TRUE){
  signs <- if(plus_neg){ifelse(x < 0, "-", "+")}else{""}
  x <- x %>%
    abs() %>%
    base::round(round) %>%
    as.character()
  x <- paste0(signs, "$", x, suffix)
  return(x)
}

format_table_pc <- function(x, suffix = "%", round = 0, plus_neg = TRUE){
  signs <- if(plus_neg){ifelse(x < 0, "-", "+")}else{""}
  x <- x %>%
    `*`(100) %>%
    abs() %>%
    base::round(round) %>%
    as.character()
  x <- paste0(signs, x, suffix)
  return(x)
}

# Highcharts options
set_hcharts_options <- function(...){

  # Dollar suffixes
  hchart_lang <- getOption("highcharter.lang")
  hchart_lang$numericSymbols <- c("k", "m", "b", "t", NULL, NULL)
  options(highcharter.lang = hchart_lang)

}
