viz_bop_line_chart <- function(data = bob)
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
                     / .data$value[.data$date == as.Date("2019-12-01")] - 1))
              tooltip = paste0(
              .data$exports_imports, "\n",
              format(.data$date, "%b %Y"), "\n",
              round2(.data$value, 1), "%")









  df %>%
    djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(round2(.data$value, 1),"%"),
      y_labels = function(x) paste0(x, "%"),
    ) +
    labs(
      title = "title",
      subtitle = "Cumulative change in export and import of good since December2020, Victoria",
     caption = paste0("Seasonally Adjusted Chain Volume Measures")
    )
}





