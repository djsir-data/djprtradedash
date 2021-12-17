viz_good_trade_line_chart <- function(data = bop) {
  df <- data %>%
    dplyr::filter(date >= as.Date("2018-12-01")) %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$goods_services == "Goods", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(value = abs(.data$value))

  latest_month <- format(max(df$date), "%B %Y")


  df <- df %>%
    dplyr::group_by(.data$exports_imports) %>%
    dplyr::mutate(
      value = 100 * (.data$value
        / .data$value[.data$date == as.Date("2019-12-01")] - 1),
      tooltip = paste0(
        .data$exports_imports, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )


  latest_export <- df %>%
    dplyr::filter(
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  title <- paste0(
    "Victoria's goods exports are ",
    dplyr::case_when(
      latest_export > 0 ~ paste0(abs(latest_export), " per cent higher than "),
      latest_export == 0 ~ "the same as ",
      latest_export < 0 ~ paste0(abs(latest_export), " per cent lower than ")
    ),
    "they were in December 2019"
  )

  caption <- paste0("ABS Balnce of Payment quarterly, Seasonally Adjusted Chain Volume Measures latest data is from ", latest_month)



  df %>%
    djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%"),
    ) +
    labs(
      title = title,
      subtitle = "Cumulative change in exports and imports of goods since December 2019 in Victoria",
      caption = caption
    )
}


viz_services_trade_line_chart <- function(data = bop) {
  df <- data %>%
    dplyr::filter(date >= as.Date("2018-12-01")) %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$goods_services == "Services", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(value = abs(.data$value))

  latest_month <- format(max(df$date), "%B %Y")

  caption <- paste0("ABS Balnce of Payment quarterly, Seasonally Adjusted Chain Volume Measures latest data is from ", latest_month)

  df <- df %>%
    dplyr::group_by(.data$exports_imports) %>%
    dplyr::mutate(
      value = 100 * (.data$value
        / .data$value[.data$date == as.Date("2019-12-01")] - 1),
      tooltip = paste0(
        .data$exports_imports, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )


  latest_export <- df %>%
    dplyr::filter(
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  title <- paste0(
    "Victoria's services exports are ",
    dplyr::case_when(
      latest_export > 0 ~ paste0(abs(latest_export), " per cent higher than "),
      latest_export == 0 ~ "the same as ",
      latest_export < 0 ~ paste0(abs(latest_export), " per cent lower than ")
    ),
    "they were in December 2019"
  )



  df %>%
    djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%"),
    ) +
    labs(
      title = title,
      subtitle = "Cumulative change in exports and imports of services since December 2019 in Victoria",
      caption = caption
    )
}

viz_service_bop_bar_chart <- function(data = bop) {
  df <- data %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$goods_services == "Services", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(value = abs(.data$value)) %>%
    dplyr::mutate(state = dplyr::case_when(
      .data$state == "Australian Capital Territory" ~
      "ACT",
      .data$state == "New South Wales" ~ "NSW",
      .data$state == "Victoria" ~ "Vic",
      .data$state == "Queensland" ~ "Qld",
      .data$state == "Northern Territory" ~ "NT",
      .data$state == "South Australia" ~ "SA",
      .data$state == "Western Australia" ~ "WA",
      .data$state == "Tasmania" ~ "Tas",
    ))


  # % change of export and export since Dec 2029
  df <- df %>%
    dplyr::group_by(.data$state, .data$exports_imports) %>%
    dplyr::mutate(value = 100 * ((.data$value / .data$value[date == as.Date("2019-12-01")]) - 1)) %>%
    dplyr::ungroup()

  latest_export <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  latest_import <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Imports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  latest_month <- format(max(df$date), "%B %Y")


  title <- dplyr::case_when(
    latest_export > 0 & latest_import > 0 ~
    paste0("Both exports and imports of services increased between December 2019 and ", latest_month, " , in Victoria"),
    latest_export > 0 & latest_import < 0 ~
    paste0("While exports of services increased, imports of goods between December 2019 and ", latest_month, ", in Victoria"),
    latest_export < 0 & latest_import < 0 ~
    paste0("Both exports and imports of services fell between December 2019 and ", latest_month, ", in Victoria"),
    latest_export < 0 & latest_import > 0 ~
    paste0("While exports of services declined, imports of goods increased between December 2019 and ", latest_month, ", in Victoria"),
    TRUE ~ "Changes in services exports and imports, in Victoria"
  )


  caption <- paste0("ABS Balnce of Payment quarterly, Seasonally Adjusted Chain Volume Measures latest data is from ", latest_month)

  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  # df<- df %>%
  #   mutate(fill_col = dplyr::if_else(
  #     .data$state %in% c("Vic", "NSW"), .data$state, "Other"
  #   ))

  # draw bar chart for all state
  df %>%
    ggplot(aes(x = .data$state, y = .data$value, fill = factor(.data$exports_imports))) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_djpr(flipped = TRUE) +
    djpr_fill_manual(2) +
    geom_text(
      position = position_dodge(width = 1),
      aes(label = paste0(round2(.data$value, 1), "%")),
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
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(1, "lines"),
      legend.direction = "horizontal",
      axis.ticks = element_blank()
    ) +
    labs(
      title = title,
      subtitle = paste0(
        "Growth in exports and imports of services between December 2019 and ",
        format(max(data$date), "%B %Y")
      ),
      caption = caption
    )
}

viz_goods_bop_bar_chart <- function(data = bop) {
  df <- data %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$goods_services == "Goods", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(value = abs(.data$value)) %>%
    dplyr::filter(
      !.data$state == "Australian Capital Territory",
      !.data$state == "Northern Territory"
    ) %>%
    dplyr::mutate(state = dplyr::case_when(
      .data$state == "New South Wales" ~ "NSW",
      .data$state == "Victoria" ~ "Vic",
      .data$state == "Queensland" ~ "Qld",
      .data$state == "South Australia" ~ "SA",
      .data$state == "Western Australia" ~ "WA",
      .data$state == "Tasmania" ~ "Tas",
    ))


  # % change of export and export since Dec 2029
  df <- df %>%
    dplyr::group_by(.data$state, .data$exports_imports) %>%
    dplyr::mutate(value = 100 * ((.data$value / .data$value[date == as.Date("2019-12-01")]) - 1)) %>%
    dplyr::ungroup()

  latest_export <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  latest_import <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Imports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  latest_month <- format(max(df$date), "%B %Y")

  title <- dplyr::case_when(
    latest_export > 0 & latest_import > 0 ~
    paste0("Both exports and imports of goods increased between December 2019 and ", latest_month, " , in Victoria"),
    latest_export > 0 & latest_import < 0 ~
    paste0("While exports of goods increased, imports of goods declined between December 2019 and ", latest_month, ", in Victoria"),
    latest_export < 0 & latest_import < 0 ~
    paste0("Both exports and imports of goods fell between December 2019 and ", latest_month, ", in Victoria"),
    latest_export < 0 & latest_import > 0 ~
    paste0("While exports of goods declined, imports of goods increased between December 2019 and ", latest_month, ", in Victoria"),
    TRUE ~ "Changes in goods exports and imports, in Victoria"
  )


  caption <- paste0("ABS Balnce of Payment quarterly, Seasonally Adjusted Chain Volume Measures latest data is from ", latest_month)

  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  # draw bar chart for all state
  df %>%
    ggplot(aes(x = .data$state, y = .data$value, fill = factor(.data$exports_imports))) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_djpr(flipped = TRUE) +
    djpr_fill_manual(2) +
    geom_text(
      position = position_dodge(width = 1),
      aes(label = paste0(round2(.data$value, 1), "%")),
      vjust = 0.5,
      colour = "black",
      hjust = 0.6,
      size = 12 / .pt
    ) +
    scale_x_discrete(expand = expansion(add = c(0.7, 0.85))) +
    djpr_y_continuous() +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      legend.position = c(0.45, 1),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(1, "lines"),
      legend.direction = "horizontal",
      axis.ticks = element_blank()
    ) +
    labs(
      title = title,
      subtitle = paste0(
        "Growth in exports and imports of goods between December 2019 and ",
        format(max(data$date), "%B %Y")
      ),
      caption = caption
    ) +
    facet_wrap(~exports_imports, ncol = 2, scales = "free_y")
}



viz_goods_export_import_line <- function(data = bop) {
  df <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$goods_services == "Goods and Services", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(value = abs(.data$value))

  # Annual growth

  df <- df %>%
    dplyr::group_by(.data$exports_imports) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / lag(.data$value, 4) - 1))
    ) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      .data$exports_imports, "\n",
      format(.data$date, "%b %Y"), "\n",
      round2(.data$value, 1), "%"
    ))

  latest_month <- format(max(df$date), "%B %Y")

  export_latest <- df %>%
    dplyr::filter(.data$exports_imports == "Exports" &
      .data$date == max(.data$date)) %>%
    dplyr::mutate(value = round2(.data$value, 1)) %>%
    dplyr::pull(.data$value)

  import_latest <- df %>%
    dplyr::filter(.data$exports_imports == "Imports" &
      .data$date == max(.data$date)) %>%
    dplyr::mutate(value = round2(.data$value, 1)) %>%
    dplyr::pull(.data$value)



  title <- dplyr::case_when(
    export_latest > import_latest ~
    paste0("Exports of goods and services grew faster than imports in the year to ", latest_month),
    export_latest < import_latest ~
    paste0("Imports of goods and services grew faster than exports in the year to ", latest_month),
    export_latest == import_latest ~
    paste0("Exports of goods and services grew at around the same pace imports in the year to ", latest_month),
    TRUE ~ paste0("Exports and imports of goods and services annual")
  )

  caption <- paste0("ABS Balnce of Payment quarterly, Seasonally Adjusted and Chain Volume Measures latest data is from ", latest_month)


  df %>%
    djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%"),
      hline = 0
    ) +
    labs(
      title = title,
      subtitle = "Annual growth in goods and services export and import in Victoria",
      caption = caption
    ) +
    facet_wrap(~exports_imports, ncol = 1, scales = "free_y")
}


table_export_import <- function(data = bop) {
  df <- data %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(value = abs(.data$value))

  current <- df %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(.data$exports_imports, .data$goods_services, .data$value) %>%
    dplyr::mutate(value = round2(.data$value, 1))


  current <- current %>%
    dplyr::rename("Current figures (millions)" = value)

  # per cent change
  df_year <- df %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::group_by(.data$exports_imports, .data$goods_services) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / lag(.data$value, 4) - 1))
    ) %>%
    dplyr::mutate(value = round2(.data$value, 1)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  df_year <- df_year %>%
    dplyr::select(.data$value) %>%
    dplyr::rename("Change in the past year (%)" = value)


  df_quarterly <- df %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::group_by(.data$exports_imports, .data$goods_services) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / lag(.data$value, 1) - 1))
    ) %>%
    dplyr::mutate(value = round2(.data$value, 1)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  df_quarterly <- df_quarterly %>%
    dplyr::select(.data$value) %>%
    dplyr::rename("Change in the latest period (%)" = value)

  # Since Covid
  df_covid <- df %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::group_by(.data$exports_imports, .data$goods_services) %>%
    dplyr::mutate(
      value = 100 * (.data$value
        / .data$value[.data$date == as.Date("2019-12-01")] - 1)
    ) %>%
    dplyr::mutate(value = round2(.data$value, 1)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  df_covid <- df_covid %>%
    dplyr::select(.data$value) %>%
    dplyr::rename("Change since COVID (%)" = value)

  df_vic <- cbind(current, df_quarterly, df_year, df_covid) %>%
    dplyr::select(.data$goods_services, .data$exports_imports, .data$`Current figures (millions)`, .data$`Change in the latest period (%)`, .data$`Change in the past year (%)`, .data$`Change since COVID (%)`) %>%
    dplyr::rename("Goods/Services" = goods_services, "Exports/Imports" = exports_imports)

  latest_month <- format(max(df$date), "%B %Y")

  df_vic %>%
    gt::gt() %>%
    gt::tab_header(paste0(title = "Victoria's Export and Imports of Goods and Services"), latest_month) %>%
    gt::tab_source_note(source_note = "Source: ABS, Balance of payment, Chain Volume measure, Change Since COVID(Since December 2019)") %>%
    gt::tab_options(column_labels.background.color = "grey")
}

viz_trade_balance_line_chart <- function(data = bop) {
  df <- data %>%
    dplyr::filter(date >= as.Date("2017-12-01")) %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(value = abs(.data$value))

  # trade balance
  df <- df %>%
    tidyr::pivot_wider(
      names_from = .data$exports_imports,
      values_from = .data$value
    ) %>%
    dplyr::mutate(value = .data$Exports - .data$Imports)


  latest_month <- format(max(df$date), "%B %Y")
  caption <- paste0("ABS Balance of Payment quarterly, Seasonally Adjusted Chain Volume Measures latest data is from ", latest_month)

  df <- df %>%
    dplyr::group_by(.data$goods_services) %>%
    dplyr::mutate(
      value = 100 * (.data$value
        / .data$value[.data$date == as.Date("2019-12-01")] - 1),
      tooltip = paste0(
        .data$goods_services, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

  total_latest <- df %>%
    dplyr::filter(.data$goods_services == "Goods and Services" &
      .data$date == max(.data$date)) %>%
    dplyr::mutate(value = round2(.data$value, 1)) %>%
    dplyr::pull(.data$value)


  title <- paste0(
    "Victorian total trade balance is ",
    dplyr::case_when(
      total_latest > 0 ~ paste0(abs(total_latest), " per cent higher than "),
      total_latest == 0 ~ "the same as ",
      total_latest < 0 ~ paste0(abs(total_latest), " per cent lower than ")
    ),
    "it was in December 2019"
  )



  df %>%
    djpr_ts_linechart(
      col_var = .data$goods_services,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%"),
      hline = 0
    ) +
    labs(
      title = title,
      subtitle = "Cumulative change in total trade balance since December 2019 in Victoria",
      caption = caption
    )
}


viz_NSW_Vic_goods_line_chart <- function(data = bop) {
  df <- data %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$goods_services == "Goods", .data$indicator == "Chain Volume Measures") %>%
    dplyr::filter(.data$state %in% c("New South Wales", "Victoria")) %>%
    dplyr::mutate(value = abs(.data$value)) %>%
    dplyr::mutate(state = dplyr::case_when(
      .data$state == "New South Wales" ~ "NSW",
      .data$state == "Victoria" ~ "Vic",
    )) %>%
    dplyr::group_by(.data$exports_imports, .data$goods_services, .data$state) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / lag(.data$value, 4) - 1))
    ) %>%
    dplyr::mutate(value = round2(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      .data$exports_imports, "\n",
      format(.data$date, "%b %Y"), "\n",
      round2(.data$value, 1), "%"
    ))



  latest_vic_export <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  latest_NSW_export <- df %>%
    dplyr::filter(
      .data$state == "NSW",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

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



  caption <- paste0("ABS Balnce of Payment quarterly, Seasonally Adjusted Chain Volume Measures latest data is from ", latest_month)

  df %>%
    djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%"),
      hline = 0
    ) +
    labs(
      title = title,
      subtitle = "Annual growth in goods exports and imports in New South Wales and VIctoria",
      caption = caption
    ) +
    facet_wrap(~state, ncol = 1, scales = "free_y")
}


viz_NSW_Vic_Services_line_chart <- function(data = bop) {
  df <- data %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$goods_services == "Services", .data$indicator == "Chain Volume Measures") %>%
    dplyr::filter(.data$state %in% c("New South Wales", "Victoria")) %>%
    dplyr::mutate(value = abs(.data$value)) %>%
    dplyr::mutate(state = dplyr::case_when(
      .data$state == "New South Wales" ~ "NSW",
      .data$state == "Victoria" ~ "Vic",
    )) %>%
    dplyr::group_by(.data$exports_imports, .data$goods_services, .data$state) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / lag(.data$value, 4) - 1))
    ) %>%
    dplyr::mutate(value = round2(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()


  latest_vic_export <- df %>%
    dplyr::filter(
      .data$state == "Vic",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  latest_NSW_export <- df %>%
    dplyr::filter(
      .data$state == "NSW",
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

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



  caption <- paste0("ABS Balnce of Payment quarterly, Seasonally Adjusted Chain Volume Measures latest data is from ", latest_month)

  df %>%
    djpr_ts_linechart(
      col_var = .data$exports_imports,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%"),
      hline = 0
    ) +
    labs(
      title = title,
      subtitle = "Annual growth in services exports and imports in NSW and Victoria",
      caption = caption
    ) +
    facet_wrap(~state, ncol = 1, scales = "free_y")
}

viz_total_bop_bar_chart <- function(data = bop) {
  df <- data %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$indicator == "Chain Volume Measures", .data$exports_imports == "Exports") %>%
    dplyr::mutate(value = abs(.data$value)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::filter(
      !.data$state == "Australian Capital Territory",
      !.data$state == "Northern Territory"
    ) %>%
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
      .data$date == max(.data$date)
    ) %>%
    dplyr::filter(.data$goods_services == "Total") %>%
    dplyr::select(.data$state, .data$value) %>%
    dplyr::mutate(rank = dplyr::min_rank(-.data$value))


  vic_rank <- latest$rank[latest$state == "Vic"]


  title <- dplyr::case_when(
    vic_rank == 1 ~ paste0("Victorian total exports of goods and services are the highes exports of any Australian state"),
    vic_rank == 2 ~ paste0("Victorian total exports of goods and services are the second highest exports of any Australian state"),
    vic_rank == 3 ~ paste0("Victorian total exports of goods and services are the third highest exports of any Australian state"),
    vic_rank <= 4 ~ paste0("Victorian total exports of goods and services are the fourth highest exports of any Australian state in the year to ", format(max(df$date), "%B %Y")),
    TRUE ~ "Victoria's total exports of goods and services compared to other states and territories"
  )


  caption <- paste0("ABS Balnce of Payment quarterly, Seasonally Adjusted Chain Volume Measures latest data is from ", latest_month)


  # draw bar chart for all state
  df %>%
    ggplot(aes(x = .data$state, y = .data$value, fill = factor(.data$goods_services))) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_djpr(flipped = TRUE) +
    djpr_fill_manual(3) +
    geom_text(
      position = position_dodge(width = 1),
      aes(label = paste0(scales::comma(round2(.data$value, 1)))),
      vjust = 0.5,
      colour = "black",
      hjust = 0,
      size = 12 / .pt
    ) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.85))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      legend.position = c(0.6, 0.6),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(1, "lines"),
      legend.direction = "horizontal",
      axis.ticks = element_blank()
    ) +
    labs(
      title = title,
      subtitle = paste0(
        "Export of goods and services in millions by Australian states ",
        format(max(data$date), "%B %Y")
      ),
      caption = caption
    )
}


viz_good_services_export_chart <- function(data = bop) {
  df <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$exports_imports == "Exports", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(goods_services = dplyr::if_else(.data$goods_services == "Goods and Services", "Total", .data$goods_services)) %>%
    dplyr::mutate(value = abs(.data$value))


  latest_month <- format(max(df$date), "%B %Y")


  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      .data$goods_services, "\n",
      format(.data$date, "%b %Y"), "\n",
      round2(.data$value, 1), "%"
    ))


  latest_export <- df %>%
    dplyr::filter(
      .data$exports_imports == "Exports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  title <- paste0(
    "Victorian goods and services export is ",
    dplyr::case_when(
      latest_export > 0 ~ paste0(abs(latest_export), " per cent higher than "),
      latest_export == 0 ~ "the same as ",
      latest_export < 0 ~ paste0(abs(latest_export), " per cent lower than ")
    ),
    "it was in December 2019"
  )

  caption <- paste0("ABS Balnce of Payment quarterly, Seasonally Adjusted Chain Volume Measures latest data is from ", latest_month)

  df %>%
    djpr_ts_linechart(
      col_var = .data$goods_services,
      label_num = paste0(scales::comma(round2(.data$value, 1))),
      # y_labels = function(x) paste0(x, "%"),
    ) +
    labs(
      title = "title",
      subtitle = "Victoria's exports of goods and services in million dollars",
      caption = caption
    )
}


viz_good_services_import_chart <- function(data = bop) {
  df <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$exports_imports == "Imports", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(goods_services = dplyr::if_else(.data$goods_services == "Goods and Services", "Total", .data$goods_services)) %>%
    dplyr::mutate(value = abs(.data$value))


  latest_month <- format(max(df$date), "%B %Y")

  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      .data$state, "\n",
      format(.data$date, "%b %Y"), "\n",
      round2(.data$value, 1), "%"
    ))



  latest_export <- df %>%
    dplyr::filter(
      .data$exports_imports == "Imports",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  title <- paste0(
    "Victorian goods and services export is ",
    dplyr::case_when(
      latest_export > 0 ~ paste0(abs(latest_export), " per cent higher than "),
      latest_export == 0 ~ "the same as ",
      latest_export < 0 ~ paste0(abs(latest_export), " per cent lower than ")
    ),
    "it was in December 2019"
  )

  caption <- paste0("ABS Balnce of Payment quarterly, Seasonally Adjusted Chain Volume Measures latest data is from ", latest_month)

  df %>%
    djpr_ts_linechart(
      col_var = .data$goods_services,
      label_num = paste0(scales::comma(round2(.data$value, 1))),
      # y_labels = function(x) paste0(x, "%"),
    ) +
    labs(
      title = "title",
      subtitle = "Victoria's imports of goods and services in millions",
      caption = caption
    )
}

viz_Vic_total_bop_bar_chart <- function(data = bop) {
  df <- data %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$state == "Victoria", .data$indicator == "Chain Volume Measures", .data$exports_imports == "Exports") %>%
    dplyr::mutate(value = abs(.data$value)) %>%
    dplyr::ungroup()

  latest_month <- format(max(df$date), "%B %Y")

  df <- df %>%
    dplyr::mutate(date = format(.data$date, "%Y")) %>%
    dplyr::group_by(.data$goods_services, .data$date) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::slice_tail(n = 5) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::mutate(goods_services = dplyr::if_else(.data$goods_services == "Goods and Services", "Total", .data$goods_services))


  df_title <- df %>%
    dplyr::filter(.data$goods_services == "Total") %>%
    dplyr::mutate(change = .data$value - lag(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$change)) %>%
    dplyr::filter(.data$date == max(.data$date))


  title <- dplyr::case_when(
    df_title$change > 0 ~ "Victorian total exports has risen over the past year",
    df_title$change < 0 ~ "Victorian total exports has fallen over the past year ",
    df_title$change == 0 ~ "Victorian total exports hasn't changed over  the past year ",
    TRUE ~ "Victoria's total export over the year"
  )

  caption <- paste0("ABS Balnce of Payment quarterly, Seasonally Adjusted Chain Volume Measures latest data is from ", latest_month)


  # draw bar chart for all state
  df %>%
    ggplot(aes(x = .data$date, y = .data$value, fill = factor(.data$goods_services))) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_djpr(flipped = TRUE) +
    djpr_fill_manual(3) +
    geom_text(
      position = position_dodge(width = 1),
      aes(label = paste0(scales::comma(round2(.data$value, 1)))),
      vjust = 0.5,
      colour = "black",
      hjust = 0,
      size = 12 / .pt
    ) +
    scale_x_discrete(expand = expansion(add = c(0.25, 0.85))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      legend.position = c(0.65, 1),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(1, "lines"),
      legend.direction = "horizontal",
      axis.ticks = element_blank()
    ) +
    labs(
      title = title,
      subtitle = "Victoria's exports of goods and services in millions dollars ",
      caption = caption
    )
}
