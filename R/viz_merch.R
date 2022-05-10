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


  p <- p +
    ggplot2::scale_y_continuous(
      label = scales::label_dollar(
        scale = 1/1e06,
        suffix = "m")
      ) +
  ggplot2::labs(
    caption = caption)


  ggiraph::ggiraph(ggobj = p)

}
