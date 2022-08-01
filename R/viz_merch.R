
highcharts_merch_explorer <- function(
    dataset = merch,
    countries = c("Thailand", "Malaysia"),
    goods = c("Medicinal and pharmaceutical products (excl. medicaments of group 542)", "Aluminium"),
    origin = "Victoria",
    facet_by = "country_dest",
    smooth = TRUE
    ) {


  # Get data and initial variable creation
  df <- dataset %>%
    dplyr::filter(
      .data$sitc %in% .env$goods,
      .data$country_dest %in% .env$countries,
      .data$origin == .env$origin
    ) %>%
    dplyr::select(sitc, country_dest, date, value) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      group = paste(.data$country_dest, .data$sitc, sep = "-"),
      sitc = as.character(.data$sitc),
      country_dest = as.character(.data$country_dest),
      value = .data$value * 1000
    )

  # Ensure all combinations of date, product and country
  df <- df %>%
    right_join(tidyr::expand(df, sitc, country_dest, date)) %>%
    mutate(value = ifelse(is.na(value), 0, value))


  # clean sitc + country
  df <- df %>%
    mutate(
      across(
        c(country_dest, sitc),
        ~stringr::str_remove_all(., "\\(.+\\)") %>%
          stringr::str_squish()
        )
      )


  # Generate faceting dimention
  if (facet_by == "country_dest") {

    y_axis_labs <- unique(df$country_dest)

    df <- df %>%
      dplyr::mutate(
        col = .data$sitc,
        y_axis = factor(country_dest) %>% as.integer() %>% `-`(1L),
        name = paste0(sitc, " - ", country_dest)
      )
  } else {

    y_axis_labs <- unique(df$sitc)

    df <- df %>%
      dplyr::mutate(
        col = .data$country_dest,
        y_axis = factor(sitc) %>% as.integer() %>% `-`(1L),
        name = paste0(country_dest, " - ", sitc)
        )
  }


  # Create multiple y axis labs list
  y_axis_labs <- y_axis_labs %>%
    lapply(function(x) list(text = x))


  # Smooth data
  if (smooth) {
    df <- df %>%
      dplyr::group_by(.data$group) %>%
      dplyr::arrange(.data$date) %>%
      dplyr::mutate(
        value = slider::slide_mean(.data$value, before = 11L) %>% round()
        ) %>%
      ungroup()
  }


  # Caption
  caption <- paste0(
    "Source: ABS.Stat Merchandise Exports data per commodity (latest data is from ",
    merch_dates$max,
    "). "
  )

  # Hacky af I'm so sorry
  df <- df %>%
    arrange(y_axis, col, date) %>%
    transmute(x = datetime_to_timestamp(date), y = value, col = col, yAxis = y_axis, name = name) %>%
    group_nest(name, yAxis, col) %>%
    mutate(
      type = "line",
      data = purrr::map(data, list_parse)
      ) %>%
    list_parse()


  # Make highchart
  highchart(type = "stock") %>%
    hc_yAxis_multiples(
      create_axis(
        length(y_axis_labs),
        title = y_axis_labs,
        turnopposite  = F
        )
      ) %>%
    hc_add_series_list(df) %>%
    hc_chart(height = 250 * length(y_axis_labs)) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_add_dependency("plugins/series-label.js") %>%
    highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(enabled = TRUE) %>%
    highcharter::hc_caption(
      text = paste0(
        "Source: ABS.Stat Merchandise Exports by Commodity (latest data is from ",
        format(merch_dates$max, "%B %Y"),
        ")."
      )
    ) %>%
    highcharter::hc_rangeSelector(
      inputEnabled = T,
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
    ) %>%
    highcharter::hc_navigator(series = list(label = list(enabled = FALSE))) %>%
    hc_navigator(enabled = FALSE) %>%
    hc_scrollbar(enabled = FALSE) %>%
    djpr_highcharts()

}



update_merch_explorer <- function(
    countries,
    goods,
    origin,
    facet_by,
    smooth,
    proxy_id = "merch_explorer",
    dataset = merch
){

  # Warning
  warning("Update merch explorer currently does not work as highcharter does not have a proxy function for axis updating\nIf you could like to develop a new proxy function, please see ./JS/message_handlers.js")

  # Start loading icon
  highchartProxy(proxy_id) %>%
    hcpxy_loading(action = "show")

  # Get data and initial variable creation
  df <- dataset %>%
    dplyr::filter(
      .data$sitc %in% .env$goods,
      .data$country_dest %in% .env$countries,
      .data$origin == .env$origin
    ) %>%
    dplyr::select(sitc, country_dest, date, value) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      sitc = as.character(.data$sitc),
      country_dest = as.character(.data$country_dest),
      value = .data$value * 1000
    )

  # Ensure all combinations of date, product and country
  df <- df %>%
    right_join(tidyr::expand(df, sitc, country_dest, date)) %>%
    mutate(value = ifelse(is.na(value), 0, value))


  # clean sitc + country
  df <- df %>%
    mutate(
      across(
        c(country_dest, sitc),
        ~stringr::str_remove_all(., "\\(.+\\)") %>%
          stringr::str_squish()
      )
    )


  # Generate faceting dimention
  if (facet_by == "country_dest") {

    y_axis_labs <- unique(df$country_dest)

    df <- df %>%
      dplyr::mutate(
        col = .data$sitc,
        y_axis = factor(country_dest) %>% as.integer() %>% `-`(1L),
        name = paste0(sitc, " - ", country_dest)
      )
  } else {

    y_axis_labs <- unique(df$sitc)

    df <- df %>%
      dplyr::mutate(
        col = .data$country_dest,
        y_axis = factor(sitc) %>% as.integer() %>% `-`(1L),
        name = paste0(country_dest, " - ", sitc)
      )
  }


  # Create multiple y axis labs list
  y_axis_labs <- y_axis_labs %>%
    lapply(function(x) list(text = x))


  # Smooth data
  if (smooth) {
    df <- df %>%
      dplyr::group_by(.data$country_dest, .data$sitc) %>%
      dplyr::arrange(.data$date) %>%
      dplyr::mutate(
        value = slider::slide_mean(.data$value, before = 11L) %>% round()
      ) %>%
      ungroup()
  }


  # Super nested list structure bcos js is whack
  df <- df %>%
    arrange(y_axis, col, date) %>%
    transmute(x = datetime_to_timestamp(date), y = value, col = col, yAxis = y_axis, name = name) %>%
    group_nest(name, yAxis, col) %>%
    mutate(
      type = "line",
      data = purrr::map(data, list_parse)
    ) %>%
    list_parse()


  # Remove all series and loading icon
  highchartProxy(proxy_id) %>%
    hcpxy_loading(action = "hide") %>%
    hcpxy_remove_series(all = TRUE)

  # Adjust axis
  # highchartProxy(proxy_id) %>%
  #   hcpxy_update_n_y_axis(length(y_axis_labs)) %>%
  #   hcpxy_update(
  #     yAxis = create_axis(
  #       length(y_axis_labs),
  #       title = y_axis_labs,
  #       turnopposite  = F
  #     )
  #   )

  highchartProxy(proxy_id) %>%
    hcpxy_update(
      chart = list(height = 200 * length(y_axis_labs)),
      yAxis = create_axis(
        length(y_axis_labs),
        title = y_axis_labs,
        turnopposite  = F
      )
    )

  # Add data
  lapply(df, function(new_series){
    highchartProxy(proxy_id) %>%
      hcpxy_add_series(
        name = new_series$name,
        yAxis = new_series$yAxis,
        type = new_series$type,
        data = new_series$data
        )
  })



}




hcpxy_update_n_y_axis <- function(proxy, n_axis){

  warning("hcpxy_update_n_y_axis is still under development and requires ./JS/message_handlers.js\nThe function will not work without further arguments passed to newAxis")


  proxy$session$sendCustomMessage(
    type = "updateNumYAxes",
    message = list(
      id = proxy$id,
      nAxes = n_axis
    )
  )

  return(proxy)
}
