viz_merch_explorer <- function(dataset,
                               countries = c("Thailand", "Malaysia"),
                               goods = "Medicinal and pharmaceutical products (excl. medicaments of group 542)",
                               origin = "Victoria",
                               facet_by = "country_dest",
                               smooth = FALSE,
                               merch_explorer_sitc) {


  all_dates <- dataset %>%
    dplyr::summarise(date = DISTINCT(date)) %>%
    dplyr::collect() %>%
    dplyr::mutate(date = lubridate::ymd(date))


  data_dates <- dataset %>%
    dplyr::summarise(
      min = min(date, na.rm = TRUE),
      max = max(date, na.rm = TRUE)
    ) |>
    dplyr::collect() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.Date))


  df <- dataset %>%
    dplyr::filter(
      .data$sitc %in% .env$goods,
      .data$country_dest %in% .env$countries,
      .data$origin == .env$origin
    )

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect() %>%
      dplyr::mutate(date = lubridate::ymd(date))
  }


  df <- df %>%
    dplyr::mutate(
      group = paste(.data$country_dest, .data$sitc, sep = "-"),
      sitc = as.character(.data$sitc),
      country_dest = as.character(.data$country_dest),
      value = .data$value * 1000
    )



  combs <- df %>% dplyr::select(-date, -value) %>% unique()


  df <- dplyr::bind_rows(
    merge(all_dates$date, combs) %>%
      dplyr::rename(date = 1) %>%
      dplyr::mutate(value = 0),
    df
  ) %>%
    dplyr::group_by(group, date) %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = tidyr::replace_na(value, 0)) # %>%
  # dplyr::filter(nchar(.data$sitc_code) %in% sitc_level)

  if (facet_by == "country_dest") {
    df <- df %>%
      dplyr::mutate(col = .data$sitc)
  } else {
    df <- df %>%
      dplyr::mutate(col = .data$country_dest)
  }

  if (smooth) {
    df <- df %>%
      dplyr::group_by(.data$group) %>%
      dplyr::arrange(.data$date) %>%
      dplyr::mutate(value = slider::slide_mean(.data$value, before = 11L))
  }

  n_col <- length(unique(df$col))
  cols <- if (n_col <= 10) {
    suppressWarnings(djprtheme::djpr_pal(n_col))
  } else {
    suppressWarnings(grDevices::colorRampPalette(djprtheme::djpr_pal(10))(n_col))
  }

  total_col_chars <- df$col %>%
    unique() %>%
    paste(collapse = " ") %>%
    nchar()

  show_legend <- dplyr::if_else(total_col_chars >= 100,
                                TRUE, FALSE
  )

  date_limits <- c(data_dates$min, data_dates$max)
  x_breaks <- djprtheme::breaks_right(
    limits = date_limits,
    n_breaks = 5
  )
  latest_month <- format(max(df$date), "%B %Y")
  caption <- paste0("Source: ABS.Stat Merchandise Exports data per commodity (latest data is from ",     latest_month, "). ")

  df <- df %>%
    dplyr::mutate( tooltip = paste0(
      .data$col, "\n",
      format(.data$date, "%b %Y"), "\n",
      djprshiny::round2(.data$value, 1), "%"))



  p <- df %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$date,
      y = .data$value,
      col = .data$col,
      group = .data$group
    )) +
    #ggplot2::geom_line() +
    ggplot2::geom_point(
      data = ~ dplyr::group_by(., .data$group) %>%
        dplyr::filter(.data$date == max(.data$date)),
      fill = "white",
      show.legend = FALSE,
      stroke = 1.5, size = 2.5, shape = 21) +
    ggiraph::geom_line_interactive(ggplot2::aes(tooltip = tooltip)) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::facet_wrap(facets = facet_by)



  if (show_legend) {
    p <- p +
      djprtheme::theme_djpr(legend = "top") +
      ggplot2::theme(
        legend.direction = "vertical",
        legend.text = ggplot2::element_text(size = 11)
      ) +
      ggplot2::scale_x_date(
        breaks = x_breaks,
        date_labels = "%b\n%Y"
      )
  } else {
    days_in_data <- as.numeric(data_dates$max - data_dates$min)
    p <- p +
      ggrepel::geom_text_repel(
        data = ~ dplyr::filter(
          .,
          .data$date == max(.data$date)
        ),
        ggplot2::aes(label = stringr::str_wrap(
          col,
          10
        )),
        size = 11 / ggplot2::.pt,
        lineheight = 0.9,
        hjust = 0,
        nudge_x = days_in_data * 0.033,
        seed = 123, show.legend = FALSE,
        direction = "y"
      ) +
      ggplot2::scale_x_date(
        expand = ggplot2::expansion(mult = c(0, 0.28)),
        breaks = x_breaks, date_labels = "%b\n%Y"
      ) +
      djprtheme::theme_djpr()

  }


  p +
    ggplot2::scale_y_continuous(
      label = scales::label_dollar(
        scale = 1/1e06,
        suffix = "m")
    ) +
    ggplot2::labs(caption = caption) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent",colour = NA_character_))

}


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
        height = 150,
        turnopposite  = F
        )
      ) %>%
    hc_add_series_list(df) %>%
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
    djpr_highcharts() %>%
    hc_elementId("merch_explorer")

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
  highchartProxy(proxy_id) %>%
    hcpxy_update(
      yAxis = create_axis(
        length(y_axis_labs),
        title = y_axis_labs,
        height = 150,
        turnopposite  = F
        )
    )

  # Add data
  lapply(df, function(new_series){
    do.call(
      hcpxy_add_series,
      c(proxy = highchartProxy(proxy_id), new_series)
    )
  })


}
