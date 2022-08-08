

# Latest period exports of goods and services by state
highcharts_total_bop_bar_chart <- function(data = bop) {

  df <- data %>%
    dplyr::filter(
      indicator == "Chain Volume Measures",
      exports_imports == "Exports",
      state != "Australian Capital Territory",
      state != "Northern Territory",
      date == max(date)
      ) %>%
    dplyr::mutate(value = abs(value)) %>%
    dplyr::collect() %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::mutate(state = dplyr::case_when(
      state == "New South Wales" ~ "NSW",
      state == "Victoria" ~ "Vic",
      state == "Queensland" ~ "Qld",
      state == "South Australia" ~ "SA",
      state == "Western Australia" ~ "WA",
      state == "Tasmania" ~ "Tas",
    )) %>%
    dplyr::mutate(goods_services = dplyr::if_else(goods_services == "Goods and Services", "Total", goods_services))


  latest_month <- format(max(df$date), "%B %Y")


  latest <- df %>%
    dplyr::filter(
      date == max(date),
      goods_services == "Total") %>%
    dplyr::select(state, value) %>%
    dplyr::mutate(rank = dplyr::min_rank(-value))


  vic_rank <- latest$rank[latest$state == "Vic"]


  title <- dplyr::case_when(
    vic_rank == 1 ~ paste0("Victoria was Australia's largest exporter in ", format(max(df$date), "the %B quarter %Y")),
    vic_rank == 2 ~ paste0("Victoria was Australia's second largest exporter in ", format(max(df$date), "the %B quarter %Y")),
    vic_rank == 3 ~ paste0("Victoria was Australia's third largest exporter in ", format(max(df$date), "the %B quarter %Y")),
    vic_rank == 4 ~ paste0("Victoria was Australia's fourth largest exporter in ", format(max(df$date), "the %B quarter %Y")),
    TRUE ~ "Victoria's exports compared to other states"
  )

  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, "). Note: Data seasonally Adjusted & Chain Volume Measures")


  # Make highchart
  df %>%
    dplyr::mutate(
      state = factor(
        state,
        levels = latest %>%
          dplyr::arrange(dplyr::desc(rank)) %>%
          dplyr::pull(state)
      ),
      goods_services = factor(
        goods_services,
        levels = c("Total", "Goods", "Services")
      ),
      value = value * 1e06
      ) %>%
    select(goods_services, state, value) %>%
    arrange(desc(state), goods_services) %>%
    hchart("bar", hcaes(state, value, group = goods_services)) %>%
    hc_plotOptions(
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          formatter = JS("function () {return '$' + Highcharts.numberFormat(this.point.y / 1000000000, 1) + 'b';}")
          )
        )
      ) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}"),
      tickAmount = 6,
      accessibility = list(description = "Exports in Australian dollars")
    ) %>%
    highcharter::hc_xAxis(title = list(text = NULL)) %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(enabled = TRUE) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = "Export of goods and services by Australian state") %>%
    highcharter::hc_caption(text = caption) %>%
    hc_tooltip(valuePrefix = "$") %>%
    djpr_highcharts()
}

# Cumulative Change in Victoria's goods exports and imports since COVID
highcharts_good_trade_line_chart <- function(data = bop) {

  df <- data %>%
    dplyr::filter(
      goods_services == "Goods",
      indicator == "Chain Volume Measures",
      state == "Victoria"
      )


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df<- df %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::mutate(value = abs(value))

  latest_month <- format(max(df$date), "%B %Y")
  year_prior <- format(max(df$date) %m-% months(12), "%B %Y") # https://lubridate.tidyverse.org/reference/mplus.html


  df <- df %>%
    dplyr::group_by(exports_imports) %>%
    dplyr::arrange(date)%>%
    dplyr::mutate(
      value = 100 * ((value
                      / dplyr::lag(value,4)) - 1)) %>%

    dplyr::filter(!is.na(value)) %>%
    dplyr::ungroup()

  latest_export <- df %>%
    dplyr::filter(
      exports_imports == "Exports",
      date == max(date)
    ) %>%
    dplyr::pull(value) %>%
    djprshiny::round2(1)

  title <- paste0(
    "Victoria's goods exports are ",
    dplyr::case_when(
      latest_export > 0 ~ paste0(abs(latest_export), " per cent higher than "),
      latest_export == 0 ~ "the same as ",
      latest_export < 0 ~ paste0(abs(latest_export), " per cent lower than ")
    ),
    "they were in ",year_prior #
  )

  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")

  highchart(type = "stock") %>%
    hc_add_series(
      df %>% select(exports_imports, date, value),
      "line",
      hcaes(y = round(value), x = date, group = exports_imports)
      ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_yAxis(
      title = list(text = "Cumulative annual change"),
      labels = list(format = "{text}%"),
      tickAmount = 6,
      plotLines  = list(list(
        zIndex = 2,
        value = 0,
        dashStyle = "LongDash"
      ))
    ) %>%
    #highcharter::hc_add_dependency("plugins/series-label.js") %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(enabled = TRUE) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(
      text = paste0(
        "Cumulative annual change in Victorian goods exports and imports in ",
        latest_month,
        " (%)"
        )
      ) %>%
    highcharter::hc_caption(text = caption) %>%
    djpr_highcharts() %>%
    highcharter::hc_rangeSelector(
      inputEnabled = F,
      selected = 2,
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
    hc_tooltip(valueSuffix = "%")
}

# Cumulative change in Victoria's Services' exports and imports since COVID
highcharts_services_trade_line_chart <- function(data = bop) {

  df <- data %>%
    dplyr::filter(
                  goods_services == "Services",
                  indicator == "Chain Volume Measures",
                  state == "Victoria"
    )

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df<- df %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::mutate(value = abs(value))

  latest_month <- format(max(df$date), "%B %Y")
  year_prior <- format(max(df$date) %m-% months(12), "%B %Y")


  df <- df %>%
    dplyr::group_by(exports_imports) %>%
    dplyr::arrange(date)%>%
    dplyr::mutate(
      value = 100 * ((value
                              / dplyr::lag(value,4)) - 1)) %>%

      dplyr::filter(!is.na(value)) %>%
      dplyr::ungroup()

  df <- df %>%
       dplyr::mutate( tooltip = paste0(
        exports_imports, "\n",
        format(date, "%b %Y"), "\n",
        djprshiny::round2(value, 1), "%"))

  latest_export <- df %>%
    dplyr::filter(
      exports_imports == "Exports",
      date == max(date)
    ) %>%
    dplyr::pull(value) %>%
    djprshiny::round2(1)

  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")

  title <- paste0(
    "Victoria's services exports are ",
    dplyr::case_when(
      latest_export > 0 ~ paste0(abs(latest_export), " per cent higher than "),
      latest_export == 0 ~ "the same as ",
      latest_export < 0 ~ paste0(abs(latest_export), " per cent lower than ")
    ),
    "they were in ",year_prior
  )

  highchart(type = "stock") %>%
    hc_add_series(
      df %>% select(exports_imports, date, value),
      "line",
      hcaes(y = round(value), x = date, group = exports_imports)
    ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_yAxis(
      title = list(text = "Cumulative annual change"),
      labels = list(format = "{text}%"),
      tickAmount = 6,
      plotLines  = list(list(
        zIndex = 2,
        value = 0,
        dashStyle = "LongDash"
      ))
    ) %>%
    #highcharter::hc_add_dependency("plugins/series-label.js") %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(enabled = TRUE) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(
      text = paste0(
        "Cumulative annual change in Victorian exports and imports in ",
        latest_month,
        " (%)"
      )
    ) %>%
    highcharter::hc_caption(text = caption) %>%
    djpr_highcharts() %>%
    highcharter::hc_rangeSelector(
      inputEnabled = F,
      selected = 2,
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
    )%>%
    hc_tooltip(valueSuffix = "%")

}

# Change in services exports and imports since COVID by the state
highcharts_service_bop_bar_chart <- function(data = bop) {

  last_year <- bop_dates$max - months(12)


  df <- data %>%
    dplyr::filter(
      goods_services == "Services",
      indicator == "Chain Volume Measures",
      state != "Australian Capital Territory",
      state != "Northern Territory",
      date >= !!last_year
      ) %>%
    dplyr::mutate(value = abs(value))


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

    df <- df %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::mutate(
                  state = dplyr::case_when(
                    state == "Australian Capital Territory" ~ "ACT",
                    state == "New South Wales" ~ "NSW",
                    state == "Victoria" ~ "Vic",
                    state == "Queensland" ~ "Qld",
                    state == "Northern Territory" ~ "NT",
                    state == "South Australia" ~ "SA",
                    state == "Western Australia" ~ "WA",
                    state == "Tasmania" ~ "Tas"
      ))


  # % change of export and export since Dec 2020
  df <- df %>%
    dplyr::group_by(state, exports_imports) %>%
    dplyr::arrange(date)%>%
    dplyr::mutate(value = 100 * ((value
                                  / dplyr::lag(value,4)) - 1)) %>%
    dplyr::ungroup()

  latest_export <- df %>%
    dplyr::filter(
      state == "Vic",
      exports_imports == "Exports",
      date == max(date)
    ) %>%
    dplyr::pull(value) %>%
    djprshiny::round2(1)

  latest_import <- df %>%
    dplyr::filter(
      state == "Vic",
      exports_imports == "Imports",
      date == max(date)
    ) %>%
    dplyr::pull(value) %>%
    djprshiny::round2(1)

  latest_month <- format(max(df$date), "%B %Y")
  year_prior <- format(max(df$date)%m-%months(12), "%B %Y")


  title <- dplyr::case_when(
    latest_export > 0 & latest_import > 0 ~
    paste0("Both exports and imports of services increased between ", year_prior," and ", latest_month, " in Victoria"),
    latest_export > 0 & latest_import < 0 ~
    paste0("While exports of services increased, imports of goods between ", year_prior," and ", latest_month, " in Victoria"),
    latest_export < 0 & latest_import < 0 ~
    paste0("Both exports and imports of services fell between ", year_prior," and ", latest_month, " in Victoria"),
    latest_export < 0 & latest_import > 0 ~
    paste0("While exports of services declined, imports of goods increased between ", year_prior," and ", latest_month, " in Victoria"),
    TRUE ~ "Changes in services exports and imports in Victoria"
  )

  caption <- paste0("Source: ABS Balance of Payments quarterly (latest data is from ", latest_month, "). Note: Data seasonally Adjusted & Chain Volume Measures")


  df <- df %>%
    dplyr::group_by(state) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::mutate(
      plot_order =
        ifelse(exports_imports == "Exports", value, as.numeric(NA))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(plot_order)) %>%
    dplyr::mutate(state = factor(state, levels = unique(state)))

  df %>%
    select(exports_imports, state, value) %>%
    hchart("bar", hcaes(state, round(value, 1), group = exports_imports)) %>%
    hc_plotOptions(
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          format = "{y}%"
        )
      )
    ) %>%
    highcharter::hc_yAxis(
      title = list(enabled = FALSE),
      labels = list(enabled = FALSE),
      gridLineWidth = 0,
      plotLines  = list(list(
        zIndex = 4,
        value = 0
      ))
    ) %>%
    highcharter::hc_xAxis(title = list(text = NULL)) %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(enabled = TRUE) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = paste0(
      "Growth in exports and imports of services between ", year_prior," and ",
      format(max(df$date), "%B %Y")," (%)"
    )) %>%
    highcharter::hc_caption(text = caption) %>%
    hc_tooltip(valueSuffix = "%") %>%
    djpr_highcharts()

}

# Change in goods exports and imports by the state since COVID
highcharts_goods_bop_bar_chart <- function(data = bop) {

  last_year <- bop_dates$max - months(12)

  df <- data %>%
    dplyr::filter(goods_services == "Goods",
                  indicator == "Chain Volume Measures",
                  state != "Australian Capital Territory",
                  state != "Northern Territory",
                  date >= !!last_year) %>%
    dplyr::mutate(value = abs(value))


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }


  df<- df %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::mutate(state = dplyr::case_when(
      state == "New South Wales" ~ "NSW",
      state == "Victoria" ~ "Vic",
      state == "Queensland" ~ "Qld",
      state == "South Australia" ~ "SA",
      state == "Western Australia" ~ "WA",
      state == "Tasmania" ~ "Tas"
    ))


  # % change of export and export since Dec 2019
  df <- df %>%
    dplyr::group_by(state, exports_imports) %>%
    dplyr::arrange(date)%>%
    dplyr::mutate(value = 100 * ((value
                                  / dplyr::lag(value,4)) - 1)) %>%
    dplyr::ungroup()

  latest_export <- df %>%
    dplyr::filter(
      state == "Vic",
      exports_imports == "Exports",
      date == max(date)
    ) %>%
    dplyr::pull(value) %>%
    djprshiny::round2(1)

  latest_import <- df %>%
    dplyr::filter(
      state == "Vic",
      exports_imports == "Imports",
      date == max(date)
    ) %>%
    dplyr::pull(value) %>%
    djprshiny::round2(1)

  latest_month <- format(max(df$date), "%B %Y")
  year_prior <- format(max(df$date)%m-% months(12), "%B %Y")


  title <- dplyr::case_when(
    latest_export > 0 & latest_import > 0 ~
    paste0("Both exports and imports of goods increased between ", year_prior," and ", latest_month," in Victoria"),
    latest_export > 0 & latest_import < 0 ~
    paste0("While exports of goods increased, imports of goods declined between ", year_prior," and ", latest_month, " in Victoria"),
    latest_export < 0 & latest_import < 0 ~
    paste0("Both exports and imports of goods fell between ", year_prior," and ", latest_month, " in Victoria"),
    latest_export < 0 & latest_import > 0 ~
    paste0("While exports of goods declined, imports of goods increased between ", year_prior," and ", latest_month, " in Victoria"),
    TRUE ~ "Changes in goods exports and imports in Victoria"
  )


  caption <- paste0("Source: ABS Balance of Payments quarterly (latest data is from ", latest_month, "). Note: Data seasonally Adjusted & Chain Volume Measures")


  df <- df %>%
    dplyr::group_by(state) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::mutate(
      plot_order =
        ifelse(exports_imports == "Exports", value, as.numeric(NA))
      ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(plot_order)) %>%
    dplyr::mutate(state = factor(state, levels = unique(state)))

  df %>%
    select(exports_imports, state, value) %>%
    hchart("bar", hcaes(state, round(value, 1), group = exports_imports)) %>%
    hc_plotOptions(
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          format = "{y}%"
        )
      )
    ) %>%
    highcharter::hc_xAxis(title = list(text = NULL)) %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(enabled = TRUE) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = paste0(
      "Growth in exports and imports of goods between ", year_prior," and ",
      format(max(df$date), "%B %Y")," (%)"
    )
    ) %>%
    highcharter::hc_caption(text = caption) %>%
    hc_tooltip(valueSuffix = "%") %>%
    djpr_highcharts() %>%
    highcharter::hc_yAxis(
      title = list(enabled = FALSE),
      labels = list(enabled = FALSE),
      gridLineWidth = 0,
      plotLines  = list(list(
        zIndex = 4,
        value = 0
      ))
    )
}

# Annual growth of Victoria's imports and exports of goods & services
highcharts_goods_export_import_line <- function(data = bop) {
  df <- data %>%
    dplyr::filter(
      state == "Victoria",
      goods_services == "Goods and Services",
      indicator == "Chain Volume Measures"
    )

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }


    df <- df %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::mutate(value = abs(value))

  # Annual growth

  df <- df %>%
    dplyr::group_by(exports_imports) %>%
    dplyr::mutate(
      value = 100 * ((value / dplyr::lag(value, 4) - 1))
    ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::ungroup()

  latest_month <- format(max(df$date), "%B %Y")

  export_latest <- df %>%
    dplyr::filter(exports_imports == "Exports" &
      date == max(date)) %>%
    dplyr::mutate(value = djprshiny::round2(value, 1)) %>%
    dplyr::pull(value)

  import_latest <- df %>%
    dplyr::filter(exports_imports == "Imports" &
      date == max(date)) %>%
    dplyr::mutate(value = djprshiny::round2(value, 1)) %>%
    dplyr::pull(value)



  title <- dplyr::case_when(
    export_latest > import_latest ~
    paste0("Exports grew faster than imports in the year to ", latest_month),
    export_latest < import_latest ~
    paste0("Imports grew faster than exports in the year to ", latest_month),
    export_latest == import_latest ~
    paste0("Exports grew at around the same pace imports in the year to ", latest_month),
    TRUE ~ paste0("Exports and imports of goods and services annual")
  )

  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, "). Note: Data seasonally Adjusted & Chain Volume Measures")

  highchart(type = "stock") %>%
    hc_yAxis_multiples(
      create_axis(
        turnopposite = F,
        title = list(list(text = "Exports"), list(text = "Imports")),
        labels = list(list(format = "{text}%"), list(format = "{text}%")),
        plotLines  = list(
          list(list(
            zIndex = 2,
            value = 0,
            dashStyle = "LongDash"
          )),
          list(list(
            zIndex = 2,
            value = 0,
            dashStyle = "LongDash"
          ))
        )
        )
      ) %>%
    hc_add_series(
      df %>%
        filter(exports_imports == "Exports") %>%
        select(date, value),
      "line",
      hcaes(y = round(value, 1), x = date),
      yAxis = 0,
      name = "Exports"
    ) %>%
    hc_add_series(
      df %>%
        filter(exports_imports == "Imports") %>%
        select(date, value),
      "line",
      hcaes(y = round(value, 1), x = date),
      yAxis = 1,
      name = "Imports"
    ) %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(enabled = TRUE) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(
      text = "Annual growth in Victorian goods and services trade"
    ) %>%
    highcharter::hc_caption(text = caption) %>%
    djpr_highcharts() %>%
    highcharter::hc_rangeSelector(
      inputEnabled = F,
      selected = 0,
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
    )%>%
    hc_tooltip(valueSuffix = "%")

}

# Balance of trade in goods and services since COVID
highcharts_trade_balance_line_chart <- function(data = bop) {
  df <- data %>%
    dplyr::filter(
      state == "Victoria",
      indicator == "Chain Volume Measures"
    )

    if ('tbl_lazy' %in% class(df)) {
      df <- df %>%
        dplyr::collect()
    }


  df <-df %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::mutate(value = abs(value))

  # trade balance
  df <- df %>%
    tidyr::pivot_wider(
      names_from = exports_imports,
      values_from = value
    ) %>%
    dplyr::mutate(value = Exports - Imports)


  latest_month <- format(max(df$date), "%B %Y")
  year_prior <- format(max(df$date)%m-%months(12), "%B %Y")


  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, "). Note: Data seasonally Adjusted & Chain Volume Measures")


  df <- df %>%
    dplyr::group_by(goods_services) %>%
    dplyr::mutate(
      value = 100 * ((value
                      / dplyr::lag(value,4)) - 1)) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::ungroup()


  total_latest <- df %>%
    dplyr::filter(goods_services == "Goods and Services" &
      date == max(date)) %>%
    dplyr::mutate(value = djprshiny::round2(value, 1)) %>%
    dplyr::pull(value)


  title <- paste0(
    "Victoria's total trade balance is ",
    dplyr::case_when(
      total_latest > 0 ~ paste0(abs(total_latest), " per cent higher than "),
      total_latest == 0 ~ "the same as ",
      total_latest < 0 ~ paste0(abs(total_latest), " per cent lower than ")
    ),
    "it was in ",year_prior
  )

  highchart(type = "stock") %>%
    hc_add_series(
      df %>% select(goods_services, date, value),
      "line",
      hcaes(y = round(value, 1), x = date, group = goods_services)
    ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_yAxis(
      title = list(text = "Cumulative annual change"),
      labels = list(format = "{text}%"),
      plotLines  = list(list(
        zIndex = 2,
        value = 0,
        dashStyle = "LongDash"
      ))
    ) %>%
    #highcharter::hc_add_dependency("plugins/series-label.js") %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(enabled = TRUE) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(
      text = paste0(
        "Cumulative annual change in Victorian exports and imports in ",
        latest_month,
        "(%)"
        )
    ) %>%
    highcharter::hc_caption(text = caption) %>%
    djpr_highcharts() %>%
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
    hc_tooltip(valueSuffix = "%")
}

# Annual growth of Victoria's and NSW's imports and exports of goods
highcharts_NSW_Vic_goods_line_chart <- function(data = bop) {
  df <- data %>%
    dplyr::filter(
      goods_services == "Goods",
      indicator == "Chain Volume Measures",
      state %in% c("New South Wales", "Victoria")
      ) %>%
    dplyr::mutate(value = abs(value)) %>%
    dplyr::mutate(state = dplyr::case_when(
      state == "New South Wales" ~ "NSW",
      state == "Victoria" ~ "Vic"
    ))

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }


  df<-df %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::group_by(exports_imports, goods_services, state) %>%
    dplyr::mutate(
      value = 100 * ((value / dplyr::lag(value, 4) - 1))
    ) %>%
    dplyr::mutate(value = djprshiny::round2(value, 1)) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(tooltip = paste0(
      exports_imports, "\n",
      format(date, "%b %Y"), "\n",
      djprshiny::round2(value, 1), "%"
    ))


  latest_vic_export <- df %>%
    dplyr::filter(
      state == "Vic",
      exports_imports == "Exports",
      date == max(date)
    ) %>%
    dplyr::pull(value) %>%
    djprshiny::round2(1)

  latest_NSW_export <- df %>%
    dplyr::filter(
      state == "NSW",
      exports_imports == "Exports",
      date == max(date)
    ) %>%
    dplyr::pull(value) %>%
    djprshiny::round2(1)

  latest_month <- format(max(df$date), "%B %Y")



  title <- dplyr::case_when(
    latest_vic_export > latest_NSW_export ~
    paste0("Victoria's exports of goods grew faster than NSW in the year to ", latest_month),
    latest_vic_export < latest_NSW_export ~
    paste0("Victoria's exports of goods grew lower than NSW in the year to ", latest_month),
    latest_vic_export == latest_NSW_export ~
    paste0("Victoria's exports of goods grew at the same rate as NSW in the year to  ", latest_month),
    TRUE ~ "Annual growth exporst and imports in goods"
  )


  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, "). Note: Data seasonally Adjusted & Chain Volume Measures")

  df <- df %>% arrange(state, date)

  highchart(type = "stock") %>%
    hc_yAxis_multiples(
      create_axis(
        turnopposite = F,
        title = list(list(text = "Vic"), list(text = "NSW")),
        labels = list(list(format = "{text}%"), list(format = "{text}%")),
        plotLines  = list(
          list(list(
            zIndex = 2,
            value = 0,
            dashStyle = "LongDash"
          )),
          list(list(
            zIndex = 2,
            value = 0,
            dashStyle = "LongDash"
          ))
        )
      )
    ) %>%
    hc_add_series(
      df %>%
        filter(state  == "Vic") %>%
        select(date, value, exports_imports),
      "line",
      hcaes(y = round(value, 1), x = date, group = paste("Vic.", exports_imports)),
      yAxis = 0
    ) %>%
    hc_add_series(
      df %>%
        filter(state  == "NSW") %>%
        select(date, value, exports_imports),
      "line",
      hcaes(y = round(value, 1), x = date, group = paste("NSW", exports_imports)),
      yAxis = 1
    ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    #highcharter::hc_add_dependency("plugins/series-label.js") %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(enabled = TRUE) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(
      text = "Annual growth in goods exports and imports in New South Wales and Victoria"
    ) %>%
    highcharter::hc_caption(text = caption) %>%
    djpr_highcharts() %>%
    highcharter::hc_rangeSelector(
      inputEnabled = F,
      selected = 0,
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
    )%>%
    hc_tooltip(valueSuffix = "%")
}

# Annual growth of Victoria's and NSW's imports and exports of services
highcharts_NSW_Vic_Services_line_chart <- function(data = bop) {

  df <- data %>%
    dplyr::filter(goods_services == "Services",
                  indicator == "Chain Volume Measures",
                  state %in% c("New South Wales", "Victoria")) %>%
    dplyr::mutate(value = abs(value)) %>%
    dplyr::mutate(state = dplyr::case_when(
      state == "New South Wales" ~ "NSW",
      state == "Victoria" ~ "Vic"
    ))

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df<-df %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::group_by(exports_imports, goods_services, state) %>%
    dplyr::mutate(
      value = 100 * ((value / dplyr::lag(value, 4) - 1))
    ) %>%
    dplyr::mutate(value = djprshiny::round2(value, 1)) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::ungroup()


  latest_vic_export <- df %>%
    dplyr::filter(
      state == "Vic",
      exports_imports == "Exports",
      date == max(date)
    ) %>%
    dplyr::pull(value) %>%
    djprshiny::round2(1)

  latest_NSW_export <- df %>%
    dplyr::filter(
      state == "NSW",
      exports_imports == "Exports",
      date == max(date)
    ) %>%
    dplyr::pull(value) %>%
    djprshiny::round2(1)

  latest_month <- format(max(df$date), "%B %Y")

  title <- dplyr::case_when(
    latest_vic_export > latest_NSW_export ~
    paste0("Victoria's exports of Services grew faster than NSW exports in the year to ", latest_month),
    latest_vic_export < latest_NSW_export ~
    paste0("Victoria's exports of services grew less than NSW exports in the year to ", latest_month),
    latest_vic_export == latest_NSW_export ~
    paste0("Victoria's exports of services grew at the same rate as NSW expors in the year to  ", latest_month),
    TRUE ~ "Annual growth exporst and imports in services"
  )

  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")

  highchart(type = "stock") %>%
    hc_yAxis_multiples(
      create_axis(
        turnopposite = F,
        title = list(list(text = "Vic"), list(text = "NSW")),
        labels = list(list(format = "{text}%"), list(format = "{text}%")),
        plotLines  = list(
          list(list(
            zIndex = 2,
            value = 0,
            dashStyle = "LongDash"
          )),
          list(list(
            zIndex = 2,
            value = 0,
            dashStyle = "LongDash"
          ))
        )
      )
    ) %>%
    hc_add_series(
      df %>%
        filter(state  == "Vic") %>%
        select(date, value, exports_imports),
      "line",
      hcaes(y = round(value, 1), x = date, group = paste("Vic.", exports_imports)),
      yAxis = 0
    ) %>%
    hc_add_series(
      df %>%
        filter(state  == "NSW") %>%
        select(date, value, exports_imports),
      "line",
      hcaes(y = round(value, 1), x = date, group = paste("NSW", exports_imports)),
      yAxis = 1
    ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    #highcharter::hc_add_dependency("plugins/series-label.js") %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(enabled = TRUE) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(
      text = "Annual growth in services exports and imports in NSW and Victoria"
    ) %>%
    highcharter::hc_caption(text = caption) %>%
    djpr_highcharts() %>%
    highcharter::hc_rangeSelector(
      inputEnabled = F,
      selected = 0,
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
    )%>%
    hc_tooltip(valueSuffix = "%")
}

# Victoria's historical imports and exports of goods and services
highcharts_good_services_chart <- function(data = bop) {

  df <- data %>%
    dplyr::filter(
      state == "Victoria",
      indicator == "Chain Volume Measures"
    )


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df <- df %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::mutate(goods_services = dplyr::if_else(goods_services == "Goods and Services", "Total", goods_services)) %>%
    dplyr::mutate(value = abs(value * 1000000))


  latest_month <- format(max(df$date), "%B %Y")

  latest_change <- df %>%
    dplyr::group_by(exports_imports) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(goods_services == "Total") %>%
    dplyr::mutate(change = value - dplyr::lag(value, 1)) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup()


  title <- purrr::map2_chr(
    latest_change$exports_imports,
    latest_change$change,
    ~paste0(
      .x,
      " ",
      ifelse(.y > 0, "up ", "down "),
      dollar_stat(abs(.y))
      )
  ) %>%
  paste0(collapse = "; ") %>%
  paste(latest_month, "quarterly", .) %>%
  stringr::str_to_sentence()


  caption <- paste0(
    "Source: ABS Balance of Payment quarterly (latest data is from ",
    latest_month,
    "). Note: Data seasonally Adjusted & Chain Volume Measures"
    )

  highchart(type = "stock") %>%
    hc_yAxis_multiples(
      create_axis(
        turnopposite = F,
        title = list(list(text = "Exports"), list(text = "Imports")),
        labels = list(list(format = "${text}"), list(format = "${text}"))
      )
    ) %>%
    hc_add_series(
      df %>%
        filter(exports_imports == "Exports") %>%
        select(date, value, goods_services),
      "line",
      hcaes(y = round(value, 1), x = date, group = paste(goods_services, "exports")),
      yAxis = 0
    ) %>%
    hc_add_series(
      df %>%
        filter(exports_imports == "Imports") %>%
        select(date, value, goods_services),
      "line",
      hcaes(y = round(value, 1), x = date, group = paste(goods_services, "imports")),
      yAxis = 1
    ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    #highcharter::hc_add_dependency("plugins/series-label.js") %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(enabled = TRUE) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = "Total Victorian trade volumes") %>%
    highcharter::hc_caption(text = caption) %>%
    djpr_highcharts() %>%
    highcharter::hc_rangeSelector(
      inputEnabled = F,
      selected = 0,
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
    )%>%
    hc_tooltip(valuePrefix = "$") %>%
    hc_chart(
      height = 650
    )

}



