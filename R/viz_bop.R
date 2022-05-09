#-------------------- Goods & Services -----------------------

# Latest period exports of goods and services by state
viz_total_bop_bar_chart <- function(data = bop) {

  df <- data %>%
    dplyr::filter(.data$indicator == "Chain Volume Measures",
                  .data$exports_imports == "Exports",
                  .data$state != "Australian Capital Territory",
                  .data$state != "Northern Territory",
                  .data$date == max(.data$date)) %>%
    dplyr::mutate(value = abs(.data$value))


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df <- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(state = dplyr::case_when(
      .data$state == "New South Wales" ~ "NSW",
      .data$state == "Victoria" ~ "Vic",
      .data$state == "Queensland" ~ "Qld",
      .data$state == "South Australia" ~ "SA",
      .data$state == "Western Australia" ~ "WA",
      .data$state == "Tasmania" ~ "Tas",
    )) %>%
    dplyr::mutate(goods_services = dplyr::if_else(.data$goods_services == "Goods and Services", "Total", .data$goods_services))


  latest_month <- format(max(df$date), "%B %Y")


  latest <- df %>%
    dplyr::filter(
      .data$date == max(.data$date),
      .data$goods_services == "Total") %>%
    dplyr::select(.data$state, .data$value) %>%
    dplyr::mutate(rank = dplyr::min_rank(-.data$value))


  vic_rank <- latest$rank[latest$state == "Vic"]


  title <- dplyr::case_when(
    vic_rank == 1 ~ paste0("Victoria was Australia's largest exporter in ", format(max(df$date), "the %B quarter %Y")),
    vic_rank == 2 ~ paste0("Victoria was Australia's second largest exporter in ", format(max(df$date), "the %B quarter %Y")),
    vic_rank == 3 ~ paste0("Victoria was Australia's third largest exporter in ", format(max(df$date), "the %B quarter %Y")),
    vic_rank == 4 ~ paste0("Victoria was Australia's fourth largest exporter in ", format(max(df$date), "the %B quarter %Y")),
    TRUE ~ "Victoria's exports compared to other states"
  )

  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, "). Note: Data seasonally Adjusted & Chain Volume Measures")



  # draw bar chart for all state
  df %>%
    dplyr::mutate(
      state = factor(
        .data$state,
        levels = latest %>%
          dplyr::arrange(dplyr::desc(rank)) %>%
          dplyr::pull(state)
        ),
      value = .data$value * 1e06
      ) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$state, y = .data$value, fill = factor(.data$goods_services))) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::coord_flip() +
    djprtheme::theme_djpr(flipped = TRUE) +
    djprtheme::djpr_fill_manual(3) +
    ggplot2::geom_text(
      position = ggplot2::position_dodge(width = 1),
      ggplot2::aes(label = dollar_stat(.data$value)),
      vjust = 0.5,
      colour = "black",
      hjust = -0.1,
      size = 12 / ggplot2::.pt
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.2, 0.85))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      legend.position = c(0.65, 0.1),
      legend.key.height = ggplot2::unit(1, "lines"),
      legend.key.width = ggplot2::unit(1, "lines"),
      legend.direction = "horizontal",
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Export of goods and services by Australian state ($)",
      caption = caption
    )
}

# Victoria's historical exports of goods and services
viz_good_services_export_chart <- function(
  data = bop,
  dates = c(bop_dates$min, bop_dates$max)
  ) {

  df <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
      .data$exports_imports == "Exports",
      .data$indicator == "Chain Volume Measures",
      .data$date >= !!dates[1],
      .data$date <= !!dates[2]
      )

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

    df <- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(goods_services = dplyr::if_else(.data$goods_services == "Goods and Services", "Total", .data$goods_services)) %>%
    dplyr::mutate(value = abs(.data$value))


  latest_month <- format(max(df$date), "%B %Y")


  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      .data$goods_services, "\n",
      format(.data$date, "%b %Y"), "\n",
      djprshiny::round2(.data$value, 1)
    ))

  latest_change <- df %>%
    dplyr::filter(.data$goods_services == "Total") %>%
    dplyr::mutate(change = .data$value - dplyr::lag(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$change),
                  .data$date == max(.data$date))



  title <-
    dplyr::case_when(
      latest_change$change > 0 ~ paste0("Victoria's total exports rose by ", scales::comma(latest_change$change), " million dollars over the past quarter"),
      latest_change$change < 0 ~ paste0("Victoria's total exports fell by ", scales::comma(abs(latest_change$change)), " million dollars over the past quarter"),
      latest_change$change == 0 ~ "Victoria's total exports the same as over the past quarter ",
      TRUE ~ "Victoria's total exports over the past quarter"
    )


  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, "). Note: Data seasonally Adjusted & Chain Volume Measures")


  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$goods_services,
      label_num = paste0(scales::comma(djprshiny::round2(.data$value, 1))),
      y_labels = function(x) format(x, big.mark=",")
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Victoria's exports of goods and services ($m)",
      caption = caption
    )
}

#-------------------- Goods  ---------------------------------

#-------------------- Services  ------------------------------

#-------------------- Balance of Trade  ----------------------


# Cumulative Change in Victoria's goods exports and imports since COVID
viz_good_trade_line_chart <- function(
  data = bop,
  dates = c(bop_dates$min, bop_dates$max)
) {

  df <- data %>%
    dplyr::filter(date >= as.Date("2017-12-01"),
                  .data$goods_services == "Goods",
                  .data$indicator == "Chain Volume Measures",
                  .data$state == "Victoria",
                  .data$date >= !!dates[1],
                  .data$date <= !!dates[2])


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df<- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(value = abs(.data$value))

  latest_month <- format(max(df$date), "%B %Y")
  year_prior <- format(max(df$date) %m-% months(12), "%B %Y") # https://lubridate.tidyverse.org/reference/mplus.html


  df <- df %>%
    dplyr::group_by(.data$exports_imports) %>%
    dplyr::arrange(date)%>%
    dplyr::mutate(
      value = 100 * ((.data$value
                      / dplyr::lag(.data$value,4)) - 1)) %>%

    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::mutate( tooltip = paste0(
      .data$exports_imports, "\n",
      format(.data$date, "%b %Y"), "\n",
      djprshiny::round2(.data$value, 1), "%"))



  latest_export <- df %>%
    dplyr::filter(
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
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



  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = scales::percent(.data$value, scale = 1),
      y_labels = scales::label_percent(scale = 1)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Cumulative annual change in Victorian exports and imports in ",latest_month," (%)"),
      caption = caption
    )
}

# Cumulative change in Victoria's Services' exports and imports since COVID
viz_services_trade_line_chart <- function(
  data = bop,
  dates = c(bop_dates$min, bop_dates$max)
) {

  df <- data %>%
    dplyr::filter(date >= as.Date("2017-12-01"),
                  .data$goods_services == "Services",
                  .data$indicator == "Chain Volume Measures",
                  .data$state == "Victoria",
                  .data$date >= !!dates[1],
                  .data$date <= !!dates[2]
    )

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df<- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(value = abs(.data$value))

  latest_month <- format(max(df$date), "%B %Y")
  year_prior <- format(max(df$date) %m-% months(12), "%B %Y")


  df <- df %>%
    dplyr::group_by(.data$exports_imports) %>%
    dplyr::arrange(date)%>%
    dplyr::mutate(
      value = 100 * ((.data$value
                              / dplyr::lag(.data$value,4)) - 1)) %>%

      dplyr::filter(!is.na(.data$value)) %>%
      dplyr::ungroup()

  df <- df %>%
       dplyr::mutate( tooltip = paste0(
        .data$exports_imports, "\n",
        format(.data$date, "%b %Y"), "\n",
        djprshiny::round2(.data$value, 1), "%"))

  latest_export <- df %>%
    dplyr::filter(
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
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



  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(djprshiny::round2(.data$value, 1),"%"),
      y_labels = scales::label_percent(scale = 1)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Cumulative annual change in Victorian exports and imports in ", latest_month," (%)"),
      caption = caption
    )
}

# Change in services exports and imports since COVID by the state
viz_service_bop_bar_chart <- function(data = bop) {


  df <- data %>%
    dplyr::filter(.data$goods_services == "Services",
                  .data$indicator == "Chain Volume Measures",
                  .data$state != "Australian Capital Territory",
                  .data$state != "Northern Territory") %>%
    dplyr::mutate(value = abs(.data$value))


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

    df <- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(date = lubridate::ymd(date),
                  state = dplyr::case_when(
                    .data$state == "Australian Capital Territory" ~ "ACT",
                    .data$state == "New South Wales" ~ "NSW",
                    .data$state == "Victoria" ~ "Vic",
                    .data$state == "Queensland" ~ "Qld",
                    .data$state == "Northern Territory" ~ "NT",
                    .data$state == "South Australia" ~ "SA",
                    .data$state == "Western Australia" ~ "WA",
                    .data$state == "Tasmania" ~ "Tas"
      ))


  # % change of export and export since Dec 2020
  df <- df %>%
    dplyr::group_by(.data$state, .data$exports_imports) %>%
    dplyr::arrange(.data$date)%>%
    dplyr::mutate(value = 100 * ((.data$value
                                  / dplyr::lag(.data$value,4)) - 1)) %>%
    dplyr::ungroup()

  latest_export <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    djprshiny::round2(1)

  latest_import <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Imports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    djprshiny::round2(1)

  latest_month <- format(max(df$date), "%B %Y")
  year_prior <- format(max(df$date)%m-%months(12), "%B %Y")


  title <- dplyr::case_when(
    latest_export > 0 & latest_import > 0 ~
    paste0("Both exports and imports of services increased between ", year_prior," and ", latest_month, " , in Victoria"),
    latest_export > 0 & latest_import < 0 ~
    paste0("While exports of services increased, imports of goods between ", year_prior," and ", latest_month, ", in Victoria"),
    latest_export < 0 & latest_import < 0 ~
    paste0("Both exports and imports of services fell between ", year_prior," and ", latest_month, ", in Victoria"),
    latest_export < 0 & latest_import > 0 ~
    paste0("While exports of services declined, imports of goods increased between ", year_prior," and ", latest_month, ", in Victoria"),
    TRUE ~ "Changes in services exports and imports, in Victoria"
  )

  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")


  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::mutate(
      plot_order =
        ifelse(.data$exports_imports == "Exports", value, as.numeric(NA))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(plot_order) %>%
    dplyr::mutate(state = factor(.data$state, levels = unique(.data$state)))


  # draw bar chart for all state
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$state, y = .data$value, fill = factor(.data$exports_imports))) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::coord_flip() +
    djprtheme::theme_djpr(flipped = TRUE) +
    djprtheme::djpr_fill_manual(2) +
    ggplot2::geom_text(
      position = ggplot2::position_dodge(width = 1),
      ggplot2::aes(
        # y = .data$value + sign(.data$value),
        label = scales::percent(.data$value, scale = 1, accuracy =0.1),
        hjust = ifelse(.data$value > 0, -0.1, 1.1)
      ),
      # vjust = 0.5,
      colour = "black",
      # hjust = 1.1,
      size = 12 / ggplot2::.pt
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.1, 0)) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      legend.position = c(0.8, 0.1),
      legend.key.height = ggplot2::unit(1, "lines"),
      legend.key.width = ggplot2::unit(1, "lines"),
      legend.direction = "vertical",
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = paste0(
        "Growth in exports and imports of services between ", year_prior," and ",
        format(max(df$date), "%B %Y")," (%)"
      ),
      caption = caption
    )

}

# Change in goods exports and imports by the state since COVID
viz_goods_bop_bar_chart <- function(data = bop) {

  df <- data %>%
    dplyr::filter(.data$goods_services == "Goods",
                  .data$indicator == "Chain Volume Measures",
                  .data$state != "Australian Capital Territory",
                  .data$state != "Northern Territory") %>%
    dplyr::mutate(value = abs(.data$value))


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }


  df<- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::mutate(state = dplyr::case_when(
      .data$state == "New South Wales" ~ "NSW",
      .data$state == "Victoria" ~ "Vic",
      .data$state == "Queensland" ~ "Qld",
      .data$state == "South Australia" ~ "SA",
      .data$state == "Western Australia" ~ "WA",
      .data$state == "Tasmania" ~ "Tas"
    ))


  # % change of export and export since Dec 2019
  df <- df %>%
    dplyr::group_by(.data$state, .data$exports_imports) %>%
    dplyr::arrange(.data$date)%>%
    dplyr::mutate(value = 100 * ((.data$value
                                  / dplyr::lag(.data$value,4)) - 1)) %>%
    dplyr::ungroup()

  latest_export <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    djprshiny::round2(1)

  latest_import <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Imports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    djprshiny::round2(1)

  latest_month <- format(max(df$date), "%B %Y")
  year_prior <- format(max(df$date)%m-% months(12), "%B %Y")


  title <- dplyr::case_when(
    latest_export > 0 & latest_import > 0 ~
    paste0("Both exports and imports of goods increased between ", year_prior," and ", latest_month,", in Victoria"),
    latest_export > 0 & latest_import < 0 ~
    paste0("While exports of goods increased, imports of goods declined between ", year_prior," and ", latest_month, ", in Victoria"),
    latest_export < 0 & latest_import < 0 ~
    paste0("Both exports and imports of goods fell between ", year_prior," and ", latest_month, ", in Victoria"),
    latest_export < 0 & latest_import > 0 ~
    paste0("While exports of goods declined, imports of goods increased between ", year_prior," and ", latest_month, ", in Victoria"),
    TRUE ~ "Changes in goods exports and imports, in Victoria"
  )


  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")


  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::mutate(
      plot_order =
        ifelse(.data$exports_imports == "Exports", value, as.numeric(NA))
      ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(plot_order) %>%
    dplyr::mutate(state = factor(.data$state, levels = unique(.data$state)))

  # draw bar chart for all state
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$state, y = .data$value, fill = factor(.data$exports_imports))) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::coord_flip() +
    djprtheme::theme_djpr(flipped = TRUE) +
    djprtheme::djpr_fill_manual(2) +
    ggplot2::geom_text(
      position = ggplot2::position_dodge(width = 1),
      ggplot2::aes(
        # y = .data$value + sign(.data$value),
        label = scales::percent(.data$value, scale = 1,accuracy =0.1),
        hjust = ifelse(.data$value > 0, -0.1, 1.1)
        ),
      # vjust = 0.5,
      colour = "black",
      # hjust = 1.1,
      size = 12 / ggplot2::.pt
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.1, 0)) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      legend.position = c(0.8, 0.1),
      legend.key.height = ggplot2::unit(1, "lines"),
      legend.key.width = ggplot2::unit(1, "lines"),
      legend.direction = "horizontal",
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = paste0(
        "Growth in exports and imports of goods between ", year_prior," and ",
        format(max(df$date), "%B %Y")," (%)"
      ),
      caption = caption
    )
}


# Annual growth of Victoria's imports and exports of goods & services
viz_goods_export_import_line <- function(
  data = bop,
  dates = c(bop_dates$min, bop_dates$max)
) {
  df <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
      .data$goods_services == "Goods and Services",
      .data$indicator == "Chain Volume Measures",
      .data$date >= !!dates[1],
      .data$date <= !!dates[2]
    )

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }


    df <- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(value = abs(.data$value))

  # Annual growth

  df <- df %>%
    dplyr::group_by(.data$exports_imports) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / dplyr::lag(.data$value, 4) - 1))
    ) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      .data$exports_imports, "\n",
      format(.data$date, "%b %Y"), "\n",
      djprshiny::round2(.data$value, 1), "%"
    ))

  latest_month <- format(max(df$date), "%B %Y")

  export_latest <- df %>%
    dplyr::filter(.data$exports_imports == "Exports" &
      .data$date == max(.data$date)) %>%
    dplyr::mutate(value = djprshiny::round2(.data$value, 1)) %>%
    dplyr::pull(.data$value)

  import_latest <- df %>%
    dplyr::filter(.data$exports_imports == "Imports" &
      .data$date == max(.data$date)) %>%
    dplyr::mutate(value = djprshiny::round2(.data$value, 1)) %>%
    dplyr::pull(.data$value)



  title <- dplyr::case_when(
    export_latest > import_latest ~
    paste0("Exports grew faster than imports in the year to ", latest_month),
    export_latest < import_latest ~
    paste0("Imports grew faster than exports in the year to ", latest_month),
    export_latest == import_latest ~
    paste0("Exports grew at around the same pace imports in the year to ", latest_month),
    TRUE ~ paste0("Exports and imports of goods and services annual")
  )

  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")



  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(djprshiny::round2(.data$value, 1),"%"),
      y_labels = scales::label_percent(scale = 1, accuracy = 1),
      hline = 0
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Annual growth in Victorian goods and services trade (%)",
      caption = caption
    ) +
    ggplot2::facet_wrap(~exports_imports, ncol = 1, scales = "free_y")
}

# The table that shows the change in exports and imports of goods and services
table_export_import <- function(data = bop) {

  df <- data %>%
    dplyr::filter(.data$indicator == "Chain Volume Measures",
                  .data$state == "Victoria")


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
       dplyr::collect() %>%
      dplyr::mutate(date = as.Date(date))
  }

  df <- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(value = abs(.data$value))


  current <- df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(.data$exports_imports, .data$goods_services, .data$value) %>%
    dplyr::mutate(value = djprshiny::round2(.data$value, 1))


  current <- current %>%
    dplyr::rename("Current figures (millions)" = .data$value)

  # per cent change
  df_year <- df %>%
    dplyr::group_by(.data$exports_imports, .data$goods_services) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / dplyr::lag(.data$value, 4) - 1))
    ) %>%
    dplyr::mutate(value = djprshiny::round2(.data$value, 1)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  df_year <- df_year %>%
    dplyr::select(.data$value) %>%
    dplyr::rename("Change in the past year (%)" = .data$value)


  df_quarterly <- df %>%
    # dplyr::filter(
    #   .data$state == "Victoria",
    # ) %>%
    dplyr::group_by(.data$exports_imports, .data$goods_services) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / dplyr::lag(.data$value, 1) - 1))
    ) %>%
    dplyr::mutate(value = djprshiny::round2(.data$value, 1)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  df_quarterly <- df_quarterly %>%
    dplyr::select(.data$value) %>%
    dplyr::rename("Change in the latest period (%)" = .data$value)

  # Since Covid
  df_covid <- df %>%
    # dplyr::filter(
    #   .data$state == "Victoria",
    # ) %>%
    dplyr::group_by(.data$exports_imports, .data$goods_services) %>%
    dplyr::mutate(
      value = 100 * (.data$value
        / .data$value[.data$date == as.Date("2019-12-01")] - 1)
    ) %>%
    dplyr::mutate(value = djprshiny::round2(.data$value, 1)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  df_covid <- df_covid %>%
    dplyr::select(.data$value) %>%
    dplyr::rename("Change since COVID (%)" = .data$value)

  df_vic <- cbind(current, df_quarterly, df_year, df_covid) %>%
    dplyr::select(.data$goods_services, .data$exports_imports, .data$`Current figures (millions)`, .data$`Change in the latest period (%)`, .data$`Change in the past year (%)`, .data$`Change since COVID (%)`) %>%
    dplyr::rename("Goods/Services" = .data$goods_services, "Exports/Imports" = .data$exports_imports)

  latest_month <- format(max(df$date), "%B %Y")

  df_vic %>%
    flextable::flextable() %>%
    flextable::set_caption(paste0(title = "Victoria's Export and Imports of Goods and Services"), latest_month) %>%
    flextable::add_footer_lines("Source: ABS, Balance of payment, Chain Volume measure, Change Since COVID (Since December 2019)")
}

# Balance of trade in goods and services since COVID
viz_trade_balance_line_chart <- function(
  data = bop,
  dates = c(bop_dates$min, bop_dates$max)
) {
  df <- data %>%
    dplyr::filter(date >= as.Date("2017-12-01")) %>%
    dplyr::filter(
      .data$state == "Victoria",
      .data$indicator == "Chain Volume Measures",
      .data$date >= !!dates[1],
      .data$date <= !!dates[2]
    )

    if ('tbl_lazy' %in% class(df)) {
      df <- df %>%
        dplyr::collect()
    }


  df <-df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(value = abs(.data$value))

  # trade balance
  df <- df %>%
    tidyr::pivot_wider(
      names_from = .data$exports_imports,
      values_from = .data$value
    ) %>%
    dplyr::mutate(value = .data$Exports - .data$Imports)


  latest_month <- format(max(df$date), "%B %Y")
  year_prior <- format(max(df$date)%m-%months(12), "%B %Y")


  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")


  df <- df %>%
    dplyr::group_by(.data$goods_services) %>%
    dplyr::mutate(
      value = 100 * ((.data$value
                      / dplyr::lag(.data$value,4)) - 1)) %>%
        dplyr::filter(!is.na(.data$value)) %>%
        dplyr::ungroup()

  #add tooltip
  df <- df %>%
    dplyr::mutate(
  tooltip = paste0(
    .data$goods_services, "\n",
    format(.data$date, "%b %Y"), "\n",
    djprshiny::round2(.data$value, 1), "%"
  )

  )

  total_latest <- df %>%
    dplyr::filter(.data$goods_services == "Goods and Services" &
      .data$date == max(.data$date)) %>%
    dplyr::mutate(value = djprshiny::round2(.data$value, 1)) %>%
    dplyr::pull(.data$value)


  title <- paste0(
    "Victoria's total trade balance is ",
    dplyr::case_when(
      total_latest > 0 ~ paste0(abs(total_latest), " per cent higher than "),
      total_latest == 0 ~ "the same as ",
      total_latest < 0 ~ paste0(abs(total_latest), " per cent lower than ")
    ),
    "it was in ",year_prior
  )



  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$goods_services,
      label_num = paste0(djprshiny::round2(.data$value, 1),"%"),
      y_labels = scales::label_percent(scale = 1),
      hline = 0
    ) +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Cumulative annual change in Victorian exports and imports in ", latest_month, "(%)"),
      caption = caption
    )
}

# Annual growth of Victoria's and NSW's imports and exports of goods
viz_NSW_Vic_goods_line_chart <- function(
  data = bop,
  dates = c(bop_dates$min, bop_dates$max)
) {
  df <- data %>%
    dplyr::filter(.data$goods_services == "Goods",
                  .data$indicator == "Chain Volume Measures",
                  .data$state %in% c("New South Wales", "Victoria"),
                  .data$date >= !!dates[1],
                  .data$date <= !!dates[2]) %>%
    dplyr::mutate(value = abs(.data$value)) %>%
    dplyr::mutate(state = dplyr::case_when(
      .data$state == "New South Wales" ~ "NSW",
      .data$state == "Victoria" ~ "Vic"
    ))

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }


  df<-df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::group_by(.data$exports_imports, .data$goods_services, .data$state) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / dplyr::lag(.data$value, 4) - 1))
    ) %>%
    dplyr::mutate(value = djprshiny::round2(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(tooltip = paste0(
      .data$exports_imports, "\n",
      format(.data$date, "%b %Y"), "\n",
      djprshiny::round2(.data$value, 1), "%"
    ))


  latest_vic_export <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    djprshiny::round2(1)

  latest_NSW_export <- df %>%
    dplyr::filter(
      .data$state == "NSW",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
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


  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")


  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(djprshiny::round2(.data$value, 1),"%"),
      y_labels = scales::label_percent(scale = 1),
      hline = 0
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Annual growth in goods exports and imports in New South Wales and Victoria (%)",
      caption = caption
    ) +
    ggplot2::facet_wrap(~state, ncol = 1, scales = "free_y")
}

# Annual growth of Victoria's and NSW's imports and exports of services
viz_NSW_Vic_Services_line_chart <- function(
  data = bop,
  dates = c(bop_dates$min, bop_dates$max)
) {

  df <- data %>%
    dplyr::filter(.data$goods_services == "Services",
                  .data$indicator == "Chain Volume Measures",
                  .data$state %in% c("New South Wales", "Victoria"),
                  .data$date >= !!dates[1],
                  .data$date <= !!dates[2]) %>%
    dplyr::mutate(value = abs(.data$value)) %>%
    dplyr::mutate(state = dplyr::case_when(
      .data$state == "New South Wales" ~ "NSW",
      .data$state == "Victoria" ~ "Vic"
    ))

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df<-df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::group_by(.data$exports_imports, .data$goods_services, .data$state) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / dplyr::lag(.data$value, 4) - 1))
    ) %>%
    dplyr::mutate(value = djprshiny::round2(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(tooltip = paste0(
      .data$exports_imports, "\n",
      format(.data$date, "%b %Y"), "\n",
      djprshiny::round2(.data$value, 1), "%"
    ))


  latest_vic_export <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    djprshiny::round2(1)

  latest_NSW_export <- df %>%
    dplyr::filter(
      .data$state == "NSW",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    djprshiny::round2(1)

  latest_month <- format(max(df$date), "%B %Y")

  title <- dplyr::case_when(
    latest_vic_export > latest_NSW_export ~
    paste0("Victoria's exports of Services grew faster than NSW exports in the year to ", latest_month),
    latest_vic_export < latest_NSW_export ~
    paste0("Victoria's exports of services grew lower than NSW exports in the year to ", latest_month),
    latest_vic_export == latest_NSW_export ~
    paste0("Victoria's exports of services grew at the same rate as NSW expors in the year to  ", latest_month),
    TRUE ~ "Annual growth exporst and imports in services"
  )


  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")


  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(djprshiny::round2(.data$value, 1),"%"),
      y_labels = scales::label_percent(scale = 1),
      hline = 0
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Annual growth in services exports and imports in NSW and Victoria (%)",
      caption = caption
    ) +
    ggplot2::facet_wrap(~state, ncol = 1, scales = "free_y")
}



# Victoria's historical imports of goods and services
viz_good_services_import_chart <- function(
  data = bop,
  dates = c(bop_dates$min, bop_dates$max)
) {
  df <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
      .data$exports_imports == "Imports",
      .data$indicator == "Chain Volume Measures",
      .data$date >= !!dates[1],
      .data$date <= !!dates[2]
    )

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df <- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(goods_services = dplyr::if_else(.data$goods_services == "Goods and Services", "Total", .data$goods_services)) %>%
    dplyr::mutate(value = abs(.data$value)) %>%
    dplyr::mutate(tooltip = paste0(
      .data$state, "\n",
      format(.data$date, "%b %Y"), "\n",
      djprshiny::round2(.data$value, 1)
    ))


  latest_month <- format(max(df$date), "%B %Y")

  latest_change <- df %>%
    dplyr::filter(.data$goods_services == "Total") %>%
    dplyr::mutate(change = .data$value - dplyr::lag(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$change)) %>%
    dplyr::filter(.data$date == max(.data$date))


  title <-
    dplyr::case_when(
      latest_change$change > 0 ~ paste0("Victoria's total imports rose by ", scales::comma(latest_change$change), " million dollars over the past quarter"),
      latest_change$change < 0 ~ paste0("Victoria's total imports fell by ", scales::comma(abs(latest_change$change)), " million dollars over the past quarter"),
      latest_change$change == 0 ~ "Victoria's total imports the same as over the past quarter ",
      TRUE ~ "Victoria's total imports over the past quarter"
    )



  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")


  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$goods_services,
      label_num = paste0(scales::comma(djprshiny::round2(.data$value, 1))),
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Victoria's imports of goods and services ($m)",
      caption = caption
    )
}

# Victoria's historical imports and exports of goods and services
viz_good_services_chart <- function(
  data = bop,
  dates = c(bop_dates$min, bop_dates$max),
  facet_cols = TRUE
) {

  df <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
      .data$indicator == "Chain Volume Measures",
      .data$date >= !!dates[1],
      .data$date <= !!dates[2]
    )


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df <- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(goods_services = dplyr::if_else(.data$goods_services == "Goods and Services", "Total", .data$goods_services)) %>%
    dplyr::mutate(value = abs(.data$value * 1000000))


  latest_month <- format(max(df$date), "%B %Y")

  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      "<p><b> Victoian ",
      .data$goods_services,
      " ",
      .data$exports_imports,
      "</b></p><br/><p><i>",
      format(.data$date, "%b %Y"),
      "</i>",
      " - ",
      scales::dollar(.data$value, accuracy = 1.11, scale = 1e-09, suffix = "b"),
      "</p>"
    ))

  latest_change <- df %>%
    dplyr::group_by(.data$exports_imports) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::filter(.data$goods_services == "Total") %>%
    dplyr::mutate(change = .data$value - dplyr::lag(.data$value, 1)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
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
    ")\nNote: Data seasonally Adjusted & Chain Volume Measures"
    )


  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$goods_services,
      label_num = dollar_stat(value),
      y_labels = scales::label_dollar(accuracy = 1, scale = 1e-09, suffix = "b")
    ) +
    ggplot2::facet_wrap(~exports_imports, ncol = as.integer(facet_cols) + 1) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        family = "sans",
        face = "bold",
        size = 16
      )
    )+
    ggplot2::labs(
      title = title,
      subtitle = "Total Victorian trade volumes ($)",
      caption = caption
    )
}

# Victoria's exports of goods and services by calendar year
viz_Vic_total_bop_bar_chart <- function(
  data = bop,
  dates = c(bop_dates$min, bop_dates$max)
) {

  df <- data %>%
    dplyr::filter(.data$state == "Victoria",
                  .data$indicator == "Chain Volume Measures",
                  .data$exports_imports == "Exports",
                  .data$date >= !!dates[1],
                  .data$date <= !!dates[2]) %>%
    dplyr::mutate(value = abs(.data$value))


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }


  latest_month <- format(max(df$date), "%B %Y")

  df <- df %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(date = format(.data$date, "%Y")) %>%
    dplyr::group_by(.data$goods_services, .data$date) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::slice_tail(n = 5) %>%
    dplyr::mutate(goods_services = dplyr::if_else(.data$goods_services == "Goods and Services", "Total", .data$goods_services))


  latest_change <- df %>%
    dplyr::filter(.data$goods_services == "Total") %>%
    dplyr::mutate(change = .data$value - dplyr::lag(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$change)) %>%
    dplyr::filter(.data$date == max(.data$date))

  title <-
    dplyr::case_when(
      latest_change$change > 0 ~ paste0("Victoria's total exports rose by ", scales::comma(latest_change$change), " million dollars over the past year"),
      latest_change$change < 0 ~ paste0("Victoria's total exports fell by ", scales::comma(abs(latest_change$change)), " million dollars over the past year"),
      latest_change$change == 0 ~ "Victoria's total exports the same as over the past year ",
      TRUE ~ "Victoria's total exports over the past year"
    )

  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, "). Note: Data seasonally Adjusted & Chain Volume Measures")



  # draw bar chart for all state
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$value, fill = factor(.data$goods_services))) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::coord_flip() +
    djprtheme::theme_djpr(flipped = TRUE) +
    djprtheme::djpr_fill_manual(3) +
    ggplot2::geom_text(
      position = ggplot2::position_dodge(width = 1),
      ggplot2::aes(label = paste0(scales::comma(djprshiny::round2(.data$value, 1)))),
      vjust = 0.5,
      colour = "black",
      hjust = 0,
      size = 12 / ggplot2::.pt
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.25, 0.85))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      legend.position = c(0.65, 1),
      legend.key.height = ggplot2::unit(1, "lines"),
      legend.key.width = ggplot2::unit(1, "lines"),
      legend.direction = "horizontal",
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Victoria's exports of goods and services ($m) ",
      caption = caption
    )
}

# Victoria's exports of goods and services by calendar year
# CHECK
viz_vic_total_bop_cumul_line <- function(
  data = bop,
  dates = c(
    bop_dates$max - lubridate::years(5) + lubridate::days(1),
    bop_dates$max
    )
  ) {

  # filter_date <- dates$max - lubridate::years(5) + lubridate::days(1)

  df <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
      .data$indicator == "Chain Volume Measures",
      .data$exports_imports == "Exports",
      .data$goods_services == "Goods and Services",
      .data$date >= !!dates[1],
      .data$date <= !!dates[2]
      )

    if ('tbl_lazy' %in% class(df)) {
      df <- df %>%
        dplyr::collect()
    }

  df <-df %>%
    dplyr::mutate(
      value = abs(.data$value * 1e06),
      year = lubridate::year(.data$date),
      quarter = lubridate::quarter(.data$date)
      ) %>%
    dplyr::group_by(year) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(value = cumsum(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$date, .data$year, .data$quarter, .data$value)

  latest_month <- format(max(df$date), "%B %Y")

  latest_change <- df %>%
    dplyr::mutate(change = .data$value - dplyr::lag(.data$value, 4)) %>%
    dplyr::filter(!is.na(.data$change)) %>%
    dplyr::filter(.data$date == max(.data$date))



  if(latest_change$quarter == 1) {
    title <-
      dplyr::case_when(
        latest_change$change > 0 ~ paste0("Victoria's exports in Q", latest_change$quarter, " are ", dollar_stat(abs(latest_change$change)), " higher than Q1 ", (latest_change$year - 1)),
        latest_change$change < 0 ~ paste0("Victoria's exports in Q", latest_change$quarter, " are ", dollar_stat(abs(latest_change$change)), " lower than Q1 ", (latest_change$year-1)),
        latest_change$change == 0 ~ paste0("Victoria's  exports are the same as Q", latest_change$quarter, " ", (latest_change$year-1)),
        TRUE ~ "Victoria's exports over the past year"
      )
  } else {
    title <-
      dplyr::case_when(
        latest_change$change > 0 ~ paste0("Victoria's year to date exports are ", dollar_stat(abs(latest_change$change)), " higher than", (latest_change$year-1)),
        latest_change$change < 0 ~ paste0("Victoria's year to date exports are ", dollar_stat(abs(latest_change$change)), " lower than ", (latest_change$year-1)),
        latest_change$change == 0 ~ paste0("Victoria's year to date exports are the same as ", (latest_change$year-1)),
        TRUE ~ "Victoria's exports over the past year"
      )
  }


  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, "). Note: Seasonally Adjusted Chain Volume Measure Data")



  # draw line chart with relevant years
  df %>%
    dplyr::mutate(
      tooltip = paste0(
        format(.data$date, "%b %Y"),
        "\n",
        scales::dollar(.data$value, accuracy = 1.1, scale = 1/1e09, suffix = "b")
      ),
      date = lubridate::ymd(
        paste0(2021,"-", lubridate::month(.data$date),"-", "01")
        )
      ) %>%
    djprshiny::djpr_ts_linechart(
      y_var = value,
      y_labels = scales::label_dollar(accuracy = 1, scale = 1/1e09, suffix = "b"),
      col_var = factor(year),
      label = FALSE
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Victoria's cumulative exports of total goods and services ($)",
      caption = caption
    ) +
    ggplot2::scale_x_date(
      expand = ggplot2::expansion(mult = c(0, 0.2)),
      date_labels = "%B",
      breaks = as.Date(c("2021-03-01","2021-06-01", "2021-09-01","2021-12-01"))
      ) +
    ggrepel::geom_text_repel(
      direction = "y",
      hjust = -1,
      data = df %>%
        dplyr::mutate(tooltip = paste0(
          format(.data$date, "%b %Y"), "\n",
          scales::dollar(.data$value, accuracy = 1, scale = 1/1e09, suffix = "b")
        ),
        date = lubridate::ymd(paste0(2021,"-", lubridate::month(.data$date),"-", "01"))) %>%
        dplyr::group_by(year) %>%
        dplyr::filter(.data$date == max(.data$date)),
      ggplot2::aes(
        label = paste0(
          "Q",
          .data$quarter,
          " ",
          .data$year,
          " ",
          scales::dollar(.data$value, accuracy = 1.1, scale = 1/1e09, suffix = "b")
          )
        )
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_dollar(accuracy = 1, scale = 1/1e09, suffix = "b"),
      expand = c(0.2, 0.2)
      ) +
    ggplot2::scale_color_grey(start = 0.8, end = 0)

}
