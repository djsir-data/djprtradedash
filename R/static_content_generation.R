
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




# Headline goods chart
highcharts_launchpad_goods <- function(
    data  = merch,
    dates = c(merch_dates$max - months(36), merch_dates$max),
    top   = 5,
    chart_type = "spline",
    sitc_level = 3
){

  past_12_months <- merch_dates$max - months(12)

  top_5_code <- data %>%
    dplyr::filter(
      .data$country_dest == "Total",
      .data$origin == "Victoria",
      nchar(.data$sitc_code) == !!sitc_level,
      .data$date >= past_12_months,
      .data$sitc != "Total",
      substr(.data$sitc_code, 1, 1) != "9" #confidential items and misc
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
    dplyr::mutate(
      value = slider::slide_sum(value, before = 11L) * 1000
      ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      sitc_shrink = sitc %>%
        stringr::str_remove_all(
          "\\(.+\\)|and other.+|, fresh, chilled .+|, fats and waxes| and related products, nes|, inedible, except fuels| and live animals| and transport equipment|, lubricants and related materials| articles"
          ) %>%
        stringr::str_squish(),
      level_2 = substr(sitc_code, 1, 2)
    )

  level_3_data %>%
    dplyr::filter(
      .data$date >= dates[1],
      .data$date <= dates[2]
    ) %>%
    highcharter::hchart(
      chart_type,
      highcharter::hcaes(x = date, y = round(value, 0), group = sitc_shrink),
      marker = list(enabled = F)
      ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_xAxis(
      title = list(enabled = FALSE),
      accessibility = list(
        description = sprintf(
          'Date from %s to %s',
          format(dates[1], "%B %Y"),
          format(dates[2], "%B %Y")
          )
      )
      ) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}"),
      tickAmount = 6,
      accessibility = list(description = "Exports in Australian dollars")
      ) %>%
    highcharter::hc_add_dependency("plugins/series-label.js") %>%
    highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "Victorias good exports"
      ) %>%
    highcharter::hc_title(text = "Goods exports from Victoria") %>%
    highcharter::hc_subtitle(text = "Year to date goods exports of level 1 SITC classifications $AUD") %>%
    highcharter::hc_caption(
      text = paste0(
        "Source: ABS.Stat Merchandise Exports by Commodity (latest data is from ",
        format(merch_dates$max, "%B %Y"),
        ")."
        )
      ) %>%
    djpr_highcharts()
}


# Headline services chart
highcharts_launchpad_services <- function(
    data    = service_trade,
    n_years = 4,
    period  = "Calendar Year",
    chart_type = "spline"
){

  filter_years <- data %>%
    dplyr::filter(period == !!period) %>%
    dplyr::select(date) %>%
    dplyr::distinct() %>%
    dplyr::slice_max(date, n = n_years) %>%
    dplyr::collect() %>%
    dplyr::pull()

  data <- data %>%
    dplyr::filter(
      flow == "Export",
      state == "Vic.",
      period == !!period,
      date %in% !!filter_years
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      keep = level == 1,
      keep = ifelse(level_1 == "Travel" & level > 2, T, keep),
      keep = ifelse(service == "Travel", F, keep)
    ) %>%
    dplyr::filter(keep == TRUE) %>%
    dplyr::select(date, value, service)

  data %>%
    highcharter::hchart(
      chart_type,
      highcharter::hcaes(x = date, y = value, group = service),
      marker = list(enabled = F)
    ) %>%
    highcharter::hc_plotOptions(
      series = list(label = list(enabled = TRUE)),
      area = list(stacking = 'normal')
      ) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_tooltip(
      dateTimeLabelFormats = list(day = "%b %Y"),
      valuePrefix = "$"
      ) %>%
    highcharter::hc_xAxis(title = list(enabled = FALSE)) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}"),
      tickAmount = 6
    ) %>%
    highcharter::hc_add_dependency("plugins/series-label.js") %>%
    highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "Victorias service exports"
    ) %>%
    highcharter::hc_title(text = "Services exports from Victoria") %>%
    highcharter::hc_subtitle(text = "Annual value of services exported $AUD") %>%
    highcharter::hc_caption(
      text = paste0(
        "Source: ABS International Trade: Supplementary Information (latest data is from ",
        format(filter_years[1], "%b %Y"),
        ")."
      )
    ) %>%
    djpr_highcharts()
}


# Top country exports/imports
table_countries <- function(exports = merch, imports = merch_imp){

  # Determine last 12 months of data
  past_12_months <- merch_dates$max - months(12)

  # Get the top export countries by sum of 12 month value
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

  # Get the top import countries by sum of 12 month value
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

  exp <- exports %>%
    dplyr::filter(
      .data$country_dest != "Total",
      .data$origin == "Victoria",
      .data$date >= past_12_months,
      .data$sitc == "Total",
      .data$country_dest %in% top_exp
    ) %>%
    dplyr::arrange(country_dest, date) %>%
    dplyr::collect() %>%
    dplyr::group_by(country_dest) %>%
    dplyr::summarise(
      `Current monthly` = dplyr::last(value) * 1000,
      `One month change` = (dplyr::last(value) - dplyr::nth(value, -2)) / dplyr::nth(value, -2),
      `Three month change` = (dplyr::last(value) - dplyr::nth(value, -4)) / dplyr::nth(value, -4),
      `Twelve month change` = (dplyr::last(value) - dplyr::nth(value, -13)) / dplyr::nth(value, -13)
      ) %>%
    dplyr::arrange(dplyr::desc(`Current monthly`)) %>%
    dplyr::mutate(
      `Current monthly` = scales::dollar(`Current monthly`, accuracy = 1, scale = 1/1e06, suffix = "m"),
      dplyr::across(dplyr::contains("change"), ~scales::percent(., accuracy = 1)),
      country_dest = stringr::str_remove_all(country_dest, "\\(.+\\)") %>%
        stringr::str_squish() %>%
        stringr::str_replace_all("United States of America", "USA") %>%
        paste0("\t", .)
      ) %>%
    dplyr::rename(country = country_dest)

  imp <- imports %>%
    dplyr::filter(
      .data$country_origin != "Total",
      .data$dest == "Victoria",
      .data$date >= past_12_months,
      .data$sitc == "Total",
      .data$country_origin %in% top_imp
    ) %>%
    dplyr::arrange(country_origin, date) %>%
    dplyr::collect() %>%
    dplyr::group_by(country_origin) %>%
    dplyr::summarise(
      `Current monthly` = dplyr::last(value) * 1000,
      `One month change` = (dplyr::last(value) - dplyr::nth(value, -2)) / dplyr::nth(value, -2),
      `Three month change` = (dplyr::last(value) - dplyr::nth(value, -4)) / dplyr::nth(value, -4),
      `Twelve month change` = (dplyr::last(value) - dplyr::nth(value, -13)) / dplyr::nth(value, -13)
    ) %>%
    dplyr::arrange(dplyr::desc(`Current monthly`)) %>%
    dplyr::mutate(
      `Current monthly` = scales::dollar(`Current monthly`, accuracy = 1, scale = 1/1e06, suffix = "m"),
      dplyr::across(dplyr::contains("change"), ~scales::percent(., accuracy = 1)),
      country_origin = stringr::str_remove_all(country_origin, "\\(.+\\)") %>%
        stringr::str_squish() %>%
        stringr::str_replace_all("United States of America", "USA") %>%
        paste0("\t", .)
    ) %>%
    dplyr::rename(country = country_origin)

  exp_header <- as.list(c("Exports", rep("", 4)))
  imp_header <- as.list(c("Imports", rep("", 4)))

  out <- rbind(exp_header, exp, imp_header, imp) %>%
    djpr_table()

  return(out)

}


# Overall trade position
highcharts_bop_export_chart <- function(
    data = bop
) {

  df <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
      .data$exports_imports == "Exports",
      .data$indicator == "Chain Volume Measures"
    )

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df <- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(goods_services = dplyr::if_else(.data$goods_services == "Goods and Services", "Total", .data$goods_services)) %>%
    dplyr::mutate(value = abs(.data$value) * 1000000)


  latest_month <- format(max(df$date), "%B %Y")

  latest_change <- df %>%
    dplyr::filter(.data$goods_services == "Total") %>%
    dplyr::mutate(change = .data$value - dplyr::lag(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$change), .data$date == max(.data$date))



  title <-
    dplyr::case_when(
      abs(latest_change$change) < 10 ~ "Victoria's total exports remained steady over the past quarter ",
      latest_change$change > 0 ~ paste0("Victoria's total exports rose by ", dollar_stat(latest_change$change), " over the past quarter"),
      latest_change$change < 0 ~ paste0("Victoria's total exports fell by ", dollar_stat(abs(latest_change$change)), " over the past quarter"),
      TRUE ~ "Victoria's total exports over the past quarter"
    )

  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ").</br> Note: Data seasonally Adjusted & Chain Volume Measures")

  highcharter::highchart(type = "stock") %>%
    highcharter::hc_add_series(
      df,
      "line",
      highcharter::hcaes(x = date, y = value, group = goods_services),
      label = list(enabled = TRUE)
      ) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}")
    ) %>%
    highcharter::hc_add_dependency("plugins/series-label.js") %>%
    highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = "Victoria's exports of goods and services") %>%
    highcharter::hc_caption(text = caption) %>%
    highcharter::hc_rangeSelector(
      inputEnabled = F,
      selected = 1,
      buttons = list(
        list(
          type  = 'all',
          text  =  'All',
          title =  'View all'
        ),
        list(
          type  = 'year',
          count = 5,
          text  = '5y',
          title = 'View five years'
        ),
        list(
          type  = 'year',
          count = 3,
          text  = '3y',
          title = 'View three years years'
        ),
        list(
          type  = 'ytd',
          text  = 'YTD',
          title = 'View year to date'
        )
      )
    ) %>%
    highcharter::hc_navigator(series = list(label = list(enabled = FALSE))) %>%
    djpr_highcharts()

}



# Rising goods chart
highcharts_rising_goods <- function(
    data  = merch,
    top   = 5,
    chart_type = "spline",
    sitc_level = 3,
    min_annual_value = 1e06
){

  past_12_months <- merch_dates$max - months(12)

  top_5_code <- data %>%
    dplyr::filter(
      .data$country_dest == "Total",
      .data$origin == "Victoria",
      nchar(.data$sitc_code) == !!sitc_level,
      .data$sitc != "Total",
      substr(.data$sitc_code, 1, 1) != "9" #confidential items and misc
    ) %>%
    dplyr::arrange(sitc_code, date) %>%
    dplyr::collect() %>%
    dplyr::group_by(sitc_code) %>%
    dplyr::mutate(year_on_year_growth = (value - dplyr::lag(value, 12)) / dplyr::lag(value, 12)) %>%
    dplyr::filter(date >= !!past_12_months) %>%
    dplyr::summarise(mean_growth = mean(year_on_year_growth), value = sum(value)) %>%
    dplyr::filter(value >= !!min_annual_value) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(.data$mean_growth, n = top) %>%
    dplyr::select(.data$sitc_code) %>%
    dplyr::pull()

  level_3_data <- data %>%
    dplyr::filter(
      .data$country_dest == "Total",
      .data$origin == "Victoria",
      .data$sitc_code %in% !!top_5_code,
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(sitc) %>%
    dplyr::mutate(value = slider::slide_mean(value, before = 11)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      sitc_shrink = sitc %>%
        stringr::str_remove_all(
          "\\(.+\\)|and other.+|, fresh, chilled .+|, fats and waxes| and related products, nes|, inedible, except fuels| and live animals| and transport equipment|, lubricants and related materials| articles| of a kind used for the extraction of soft fixed vegetable oils"
        ) %>%
        stringr::str_squish()
    ) %>%
    dplyr::select(sitc_shrink, date, value) %>%
    dplyr::arrange(date, sitc_shrink) %>%
    dplyr::distinct()


  highcharter::highchart(type = "stock") %>%
    highcharter::hc_add_series(
      level_3_data,
      chart_type,
      highcharter::hcaes(x = date, y = round(value, 0), group = sitc_shrink),
      marker = list(enabled = F)
    ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_xAxis(
      title = list(enabled = FALSE),
      accessibility = list(
        description = sprintf(
          'Date from %s to %s',
          format(min(level_3_data$date), "%B %Y"),
          format(max(level_3_data$date), "%B %Y")
        )
      )
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}"),
      tickAmount = 6,
      accessibility = list(description = "Exports in Australian dollars")
    ) %>%
    highcharter::hc_add_dependency("plugins/series-label.js") %>%
    highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "Victorias good exports"
    ) %>%
    highcharter::hc_title(text = "Past year's top growing goods exports") %>%
    highcharter::hc_subtitle(text = "Top 5 SITC level 3 exports by average year-on-year growth $AUD") %>%
    highcharter::hc_caption(
      text = paste0(
        "Source: ABS.Stat Merchandise Exports by Commodity (latest data is from ",
        format(merch_dates$max, "%B %Y"),
        ").</br>Note: Data smoothed using 12-month rolling average."
      )
    ) %>%
    highcharter::hc_rangeSelector(
      inputEnabled = F,
      selected = 1,
      buttons = list(
        list(
          type  = 'all',
          text  =  'All',
          title =  'View all'
        ),
        list(
          type  = 'year',
          count = 5,
          text  = '5y',
          title = 'View five years'
        ),
        list(
          type  = 'year',
          count = 3,
          text  = '3y',
          title = 'View three years years'
        ),
        list(
          type  = 'ytd',
          text  = 'YTD',
          title = 'View year to date'
        )
      )
    ) %>%
    highcharter::hc_navigator(series = list(label = list(enabled = FALSE))) %>%
    djpr_highcharts()
}







