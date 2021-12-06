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

viz_good_bop_bar_chart <- function(data = bop)
{





  df <- data %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$goods_services == "Services", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(value = abs(.data$value)) %>%
    dplyr::mutate(state=dplyr::case_when(.data$state == "Australian Capital Territory"~
                                       "ACT",
                                       .data$state == "New South Wales" ~"NSW",
                                       .data$state == "Victoria"~ "Vic",
                                       .data$state == "Queensland"~ "Qld",
                                       .data$state == "Northern Territory"~"NT",
                                       .data$state == "South Australia"~ "SA",
                                       .data$state == "Western Australia"~ "WA",
                                       .data$state == "Tasmania"~ "Tas",

  ))


  # % change of export and export since Dec 2029
  df <- df %>%
    dplyr::group_by(.data$state, .data$exports_imports) %>%
    dplyr::mutate(value = 100 * ((.data$value / .data$value[date == as.Date("2019-12-01")]) - 1)) %>%
    dplyr::ungroup()

  latest_export <- df %>%
    dplyr::filter( .data$state == "Vic",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  latest_import <- df %>%
    dplyr::filter( .data$state == "Vic",
                   .data$exports_imports == "Imports",
                   .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)




  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  # draw bar chart for all state
  df %>%
    ggplot(aes(x =.data$state, y = .data$value, fill=factor(.data$exports_imports))) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_djpr(flipped = TRUE ) +
    djpr_fill_manual(2) +
    geom_text(
    position = position_dodge(width = 1),
    aes(label = paste0(round2(.data$value, 1),"%")),
    vjust = 0.5,
    colour = "black",
    hjust = 1,
    size = 12 / .pt
  ) +

  scale_x_discrete(expand = expansion(add = c(0.5, 0.85))) +
    djpr_y_continuous() +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      legend.position = c(0.2, 0.1),
      legend.key.height = unit(1.5, "lines"),
      legend.key.width = unit(1.5, "lines"),
      legend.direction = "horizontal",
      axis.ticks = element_blank()
    ) +

    labs(
      title = "title",
      subtitle = paste0(
        "Growth in export and import of services between December 2019 and ",
        format(max(data$date), "%B %Y")
      ),
      caption = paste0("Seasonally Adjusted Chain Volume Measures")
    )

}


