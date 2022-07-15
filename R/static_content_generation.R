

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





highcharts_launchpad_chart <- function(
    data  = merch,
    dates = c(merch_dates$max - months(36), merch_dates$max),
    top   = 5
){

  past_12_months <- merch_dates$max - months(12)

  top_5_code <- data %>%
    dplyr::filter(
      .data$country_dest == "Total",
      .data$origin == "Victoria",
      nchar(.data$sitc_code) == 3,
      .data$date >= past_12_months,
      .data$sitc != "Total",
      .data$sitc_code != "988" #confidential items
    ) %>%
    dplyr::group_by(sitc_code) %>%
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(.data$value, n = top) %>%
    dplyr::select(.data$sitc_code) %>%
    dplyr::collect() %>%
    dplyr::pull()

  level_3_data <- data %>%
    dplyr::filter(
      .data$country_dest == "Total",
      .data$origin == "Victoria",
      .data$sitc_code %in% !!top_5_code,
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(sitc) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(value = slider::slide_mean(value, before = 11L) * 1000) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      sitc_shrink = sitc %>%
        str_remove_all("\\(.+\\)|and other.+|, fresh, chilled .+") %>%
        str_squish(),
      level_2 = substr(sitc_code, 1, 2)
    )

  level_3_data %>%
    dplyr::filter(
      .data$date >= dates[1],
      .data$date <= dates[2]
    ) %>%
    highcharter::hchart(
      "line",
      highcharter::hcaes(x = date, y = round(value, 0), group = sitc_shrink),
      color = djprtheme::djpr_pal(top),
      marker = list(enabled = F)
      ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_xAxis(title = list(enabled = FALSE)) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}"),
      tickAmount = 6
      ) %>%
    highcharter::hc_add_dependency("plugins/series-label.js") %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "Victorias top 5 exports (SITC level 3)"
      ) %>%
    highcharter::hc_title(text = "Top 5 goods exports from Victoria") %>%
    highcharter::hc_subtitle(text = "Top SITC level 3 exports by 12-month total exports, smoothed with 12-month rolling average") %>%
    highcharter::hc_caption(
      text = paste0(
        "Source: ABS.Stat Merchandise Exports by Commodity (latest data is from",
        format(merch_dates$max, "%B %Y"),
        ")."
        )
      )
}


table_countries <- function(exports = merch, imports = merch_imp){

  past_12_months <- merch_dates$max - months(12)

  top_exp <- exports %>%
    dplyr::filter(
      .data$country_dest != "Total",
      .data$origin == "Victoria",
      .data$date >= past_12_months,
      .data$sitc == "Total"
    ) %>%
    dplyr::group_by(country_dest) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(.data$value, n = 5) %>%
    dplyr::select(.data$country_dest) %>%
    dplyr::collect() %>%
    dplyr::pull()

  top_imp <- imports %>%
    dplyr::filter(
      .data$country_origin != "Total",
      .data$dest == "Victoria",
      .data$date >= past_12_months,
      .data$sitc == "Total"
    ) %>%
    dplyr::group_by(country_origin) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(.data$value, n = 5) %>%
    dplyr::select(.data$country_origin) %>%
    dplyr::collect() %>%
    dplyr::pull()

}







