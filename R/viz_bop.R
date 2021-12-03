viz_good_bop_line_chart <- function(data = bop)
                               {


  df <- data %>%
    dplyr::filter(date >= as.Date("2018-12-01")) %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$goods_services == "Goods", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(value = abs(.data$value))




  df <- df %>%
    dplyr::group_by(.data$exports_imports) %>%
    dplyr::mutate(
      value = 100*(.data$value
                     / .data$value[.data$date == as.Date("2019-12-01")] - 1),
              tooltip = paste0(
              .data$exports_imports, "\n",
              format(.data$date, "%b %Y"), "\n",
              round2(.data$value, 1), "%"))


  latest_export <- df %>%
                dplyr::filter(
                  .data$exports_imports == "Exports",
                  #.data$exports_imports == "Export",
                  .data$date == max(.data$date)
                ) %>%
                dplyr::pull(.data$value) %>%
                round2(1)

    title <- paste0(
                "Victorian goods export is ",
                dplyr::case_when(
                  latest_export  > 0 ~ paste0(latest_export , " per cent higher than "),
                  latest_export  == 0 ~ "the same as ",
                  latest_export  < 0 ~ paste0(latest_export , " per cent lower than ")
                ),
                "it was in December 2019"
              )


  df %>%
    djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(round2(.data$value, 1),"%"),
      y_labels = function(x) paste0(x, "%"),
    ) +
    labs(
      title = title,
      subtitle = "Cumulative change in export and import of goods since December 2019 in Victoria",
     caption = paste0("Seasonally Adjusted Chain Volume Measures")
    )
}


viz_services_bop_line_chart <- function(data = bop)
                                              {


  df <- data %>%
    dplyr::filter(date >= as.Date("2018-12-01")) %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$goods_services == "Services", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(value = abs(.data$value))




  df <- df %>%
    dplyr::group_by(.data$exports_imports) %>%
    dplyr::mutate(
      value = 100*(.data$value
                   / .data$value[.data$date == as.Date("2019-12-01")] - 1),
      tooltip = paste0(
        .data$exports_imports, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"))


  latest_export <- df %>%
    dplyr::filter(
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  title <- paste0(
    "Victorian services export is ",
    dplyr::case_when(
      latest_export  > 0 ~ paste0(latest_export , " per cent higher than "),
      latest_export  == 0 ~ "the same as ",
      latest_export  < 0 ~ paste0(latest_export , " per cent lower than ")
    ),
    "it was in December 2019"
  )



  df %>%
    djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(round2(.data$value, 1),"%"),
      y_labels = function(x) paste0(x, "%"),
    ) +
    labs(
      title = title,
      subtitle = "Cumulative change in export and import of services since December 2019 in Victoria",
      caption = paste0("Seasonally Adjusted Chain Volume Measures")
    )
}


