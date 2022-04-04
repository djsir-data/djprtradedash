
viz_country_top_exp <- function(data, data_imp, country_select, chart_top_n = 4){

  data <- data %>%
    dplyr::filter(
      nchar(.data$sitc_code) == 1,
      .data$country_dest == !!country_select,
      .data$origin == "Victoria",
      .data$export_import == "export"
    ) %>%
    dplyr::collect()


  data_imp <- data_imp %>%
    dplyr::filter(
      nchar(.data$sitc_code) == 1,
      .data$date >= min(data$date, na.rm = TRUE),
      .data$date <= max(data$date, na.rm = TRUE),
      .data$country_origin == country_select,
      .data$dest == "Victoria",
      .data$export_import == "import"
    ) %>%
    dplyr::collect()

  if(!(country_select %in% data$country_dest)) return(
    data_unavil_ggplot("No data available for\n", country_select)
  )


  if(!(country_select %in% data_imp$country_origin)) return(
    data_unavil_ggplot("No data available for\n", country_select)
  )

  if(nrow(data) == 0 | nrow(data_imp) == 0) return(
    data_unavil_ggplot("No data available for\n", country_select)
    )

  current_top_sitc <- data %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::slice_max(.data$value, n = chart_top_n) %>%
    dplyr::pull(.data$sitc)

  current_top_sitc_imp <- data_imp %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::slice_max(.data$value, n = chart_top_n) %>%
    dplyr::pull(.data$sitc)

  data <- data %>%
    dplyr::mutate(
      label = ifelse(.data$sitc %in% current_top_sitc, .data$sitc, "Other") %>%
        dplyr::recode(
          "Food and live animals" = "Food",
          "Beverages and tobacco" = "Drinks and tobacco",
          "Crude materials, inedible, except fuels" = "Crude materials",
          "Mineral fuels, lubricants and related materials" = "Fuels",
          "Animal and vegetable oils, fats and waxes" = "Oils, fats and waxes",
          "Chemicals and related products, nes" = "Chemicals",
          "Manufactured goods classified chiefly by material" = "Manufactured goods",
          "Machinery and transport equipment" = "Machinery",
          "Miscellaneous manufactured articles" = "Misc. manufactured",
          "Commodities and transactions not classified elsewhere in the SITC" = "Other"
        )
    ) %>%
    dplyr::group_by(.data$date, .data$label, .data$export_import) %>%
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      value = .data$value * 1000 / 1000000,
      tip = sprintf(
        "<br><b>%s - %s</b><br><p><i>Total exports: $%s</i></p>",
        .data$label,
        format.Date(.data$date, "%b %Y"),
        format(.data$value, big.mark = ",")
        )
    )

  data_imp <- data_imp %>%
    dplyr::mutate(
      label =
        ifelse(.data$sitc %in% current_top_sitc_imp, .data$sitc, "Other") %>%
        dplyr::recode(
          "Food and live animals" = "Food",
          "Beverages and tobacco" = "Drinks and tobacco",
          "Crude materials, inedible, except fuels" = "Crude materials",
          "Mineral fuels, lubricants and related materials" = "Fuels",
          "Animal and vegetable oils, fats and waxes" = "Oils, fats and waxes",
          "Chemicals and related products, nes" = "Chemicals",
          "Manufactured goods classified chiefly by material" = "Manufactured goods",
          "Machinery and transport equipment" = "Machinery",
          "Miscellaneous manufactured articles" = "Misc. manufactured",
          "Commodities and transactions not classified elsewhere in the SITC" = "Other"
        )
    ) %>%
    dplyr::group_by(.data$date, .data$label, .data$export_import) %>%
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      value = .data$value * 1000 / 1000000,
      tip = sprintf(
        "<br><b>%s - %s</b><br><p><i>Total imports: $%s</i></p>",
        .data$label,
        format.Date(.data$date, "%b %Y"),
        format(.data$value, big.mark = ",")
      )
    )

  ranking <- data %>%
    dplyr::filter(.data$label != "Other") %>%
    dplyr::group_by(.data$label) %>%
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(.data$value)) %>%
    dplyr::pull(.data$label)

  ranking_imp <- data_imp %>%
    dplyr::filter(.data$label != "Other") %>%
    dplyr::group_by(.data$label) %>%
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(.data$value)) %>%
    dplyr::pull(.data$label)

  ranking <- c(
    ranking,
    ranking_imp[!(ranking_imp %in% ranking)],
    "Other"
  )

  data <- data %>%
    dplyr::bind_rows(data_imp) %>%
    dplyr::mutate(
      label = factor(.data$label, levels = ranking),
      export_import = stringr::str_to_title(.data$export_import)
    )

  current_top <- data %>%
    dplyr::filter(.data$date == max(.data$date))

  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = date,
      y = value,
      fill = label,
      colour = label,
      group = label
      )
    ) +
    ggiraph::geom_col_interactive(
      colour = "transparent",
      ggplot2::aes(tooltip = tip)
      ) +
    ggplot2::geom_text(
      data = current_top,
      ggplot2::aes(x = date + 25, label = label),
      position = "stack",
      hjust = 0,
      vjust = 1
      )+
    djprtheme::theme_djpr() +
    djprtheme::djpr_fill_manual(length(ranking)) +
    djprtheme::djpr_colour_manual(length(ranking)) +
    ggplot2::xlab(NULL) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0, 0.05, 0),
      labels = scales::dollar_format(1, suffix = "m")
      ) +
    ggplot2::scale_x_date(expand = c(0.05, 0, 0.3, 0)) +
    ggplot2::facet_wrap(~export_import, ncol = 1, scales = "free") +
    ggplot2::theme(strip.text = ggplot2::element_text(
      face = "bold",
      colour = "#004676",
      size = 16,
      hjust = 0,
    )
    )
}


vis_country_top_products <- function(
  data,
  country_select,
  donut_width = 4,
  sitc_digits = 2
  ){
  data <- data %>%
    dplyr::filter(
      nchar(.data$sitc_code) == !!sitc_digits,
      .data$sitc_code != "TOT",
      .data$country_dest == country_select |
        .data$country_origin == country_select,
      .data$origin == "Victoria" |
        .data$dest == "Victoria"
    ) %>%
    dplyr::collect()

  if(!(country_select %in% data$country_dest)) return(
    data_unavil_ggplot("No data available for\n", country_select)
  )

  if(!(country_select %in% data$country_origin)) return(
    data_unavil_ggplot("No data available for\n", country_select)
  )

  if(nrow(data) == 0) return(
    data_unavil_ggplot("No data available for\n", country_select)
  )

  data <- data %>%
    dplyr::group_by(.data$export_import, .data$sitc) %>%
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE) * 1000 / 1e06) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$export_import) %>%
    dplyr::slice_max(.data$value, n = 5) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(value) %>%
    dplyr::mutate(
      sitc = stringr::str_wrap(sitc),
      sitc = factor(sitc, levels = unique(sitc)),
      csum = rev(cumsum(rev(value))),
      pos = value/2 + dplyr::lead(csum, 1),
      pos = dplyr::if_else(is.na(pos), value/2, pos),
      export_import = dplyr::recode(
        .data$export_import,
        export = "Top exports",
        import = "Top imports"
      )
      )

  n_colours <- length(levels(data$sitc))

  ggplot2::ggplot(data, ggplot2::aes(x = donut_width, y = value, fill = sitc)) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(c(0.2, donut_width + 0.5)) +
    ggrepel::geom_text_repel(
      ggplot2::aes(y = pos, label = sitc),
      position = ggplot2::position_fill(-1),
      size = 4.5
      ) +
    ggplot2::facet_wrap(~export_import, nrow = 1) +
    ggplot2::theme_void() +
    djprtheme::djpr_fill_manual(n_colours) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme(strip.text = ggplot2::element_text(
      face = "bold",
      colour = "#004676",
      size = 16
    )
    )
}



viz_country_1y_exp_stat <- function(data, country_select){
  data %>%
    dplyr::filter(
      .data$country_dest == country_select,
      .data$origin == "Victoria",
      .data$export_import == "export",
      .data$sitc == "Total"
      ) %>%
    dplyr::filter(
      .data$date > max(.data$date) - lubridate::years(1)
    ) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE) * 1000) %>%
    dplyr::pull(value) %>%
    dollar_stat()
}

viz_country_1y_imp_stat <- function(data, country_select){
  data %>%
    dplyr::filter(
      .data$country_origin == country_select,
      .data$dest == "Victoria",
      .data$export_import == "import",
      .data$sitc == "Total"
    ) %>%
    dplyr::filter(
      .data$date > max(.data$date) - lubridate::years(1)
    ) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE) * 1000) %>%
    dplyr::pull(value) %>%
    dollar_stat()
}

viz_country_1y_exp_change_stat <- function(data, country_select){
  data %>%
    dplyr::filter(
      .data$country_dest == country_select,
      .data$origin == "Victoria",
      .data$export_import == "export",
      .data$sitc == "Total"
    ) %>%
    dplyr::filter(
      .data$date == max(.data$date) |
      .data$date == max(.data$date) - lubridate::years(1)
    ) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::summarise(value = (value[2] - value[1]) / value[1]) %>%
    dplyr::pull(value) %>%
    scales::percent()
}

