library(tidyverse)
library(highcharter)


highcharts_launchpad_chart <- function(
    data       = merch,
    dates      = c(merch_dates$min, merch_dates$max),
    country    = "Total",
    region     = "Victoria",
    code_level = 3,
    top        = 5,
    smooth     = TRUE
){

  filtered <- data %>%
    dplyr::filter(
      .data$country_dest %in% !!country,
      .data$origin %in% region,
      nchar(.data$sitc_code) == .env$code_level,
      .data$date >= !!dates[1],
      .data$date <= !!dates[2]
    )

  top_5_code <- filtered %>%
    dplyr::filter(.data$date == max(.data$date), .data$sitc != "Total") %>%
    dplyr::slice_max(.data$value, n = top) %>%
    dplyr::select(.data$sitc_code) %>%
    dplyr::collect() %>%
    dplyr::pull() %>%
    unique() %>%
    as.matrix()

  df <- filtered %>%
    dplyr::filter(.data$sitc_code %in% !!top_5_code) %>%
    dplyr::select(.data$sitc, .data$date, .data$value, .data$sitc_code) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      sitc_shrink = sitc %>%
        str_remove_all("\\(.+\\)|and other.+") %>%
        str_squish()
    )



  latest_month <- format(max(df$date, na.rm = TRUE), "%B %Y")

  if(smooth) {

    all_dates <- df %>%
      dplyr::select(.data$date) %>%
      unique()

    df <- df %>%
      tidyr::expand(.data$sitc, .env$all_dates) %>%
      dplyr::left_join(df, by = c("sitc", "date")) %>%
      dplyr::group_by(.data$sitc) %>%
      dplyr::mutate(value = tidyr::replace_na(.data$value, 0),
                    value = slider::slide_mean(.data$value, before = 11L)) %>%
      dplyr::ungroup()
  }


  caption <- paste0(
    "Source: ABS.Stat Merchandise Exports by Commodity (latest data is from ",
    latest_month,
    ")."
  )

  if(smooth) {
    caption <- paste0(
      caption,
      "Data has been smoothed using 12-month rolling averages."
    )
  }

  df <- df %>%
    dplyr::mutate(
      value   = .data$value / 1000,
      tooltip = paste0(
        "SITC: ",
        .data$sitc_code,
        "\n",
        format(.data$date, "%b %Y"),
        "\n",
        format(djprshiny::round2(.data$value, 1), big.mark=",")
      )
    )

  df %>%
    highcharter::hchart(
      "line",
      highcharter::hcaes(x = date, y = value, group = sitc_shrink),
      color = djprtheme::djpr_pal(5)
      ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_xAxis(title = list(enabled = FALSE)) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}k"),
      tickAmount = 6
      ) %>%
    highcharter::hc_add_dependency("plugins/series-label.js")
}

