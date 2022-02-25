page_bopUI <- function(...) {
  djpr_tab_panel(
    title = "Balance of Payments",
    h1("Key indicators"),
    paste0("This page contains Victoria's international transactions, typically quarterly or
    yearly, over a particular period.  It shows the sum of the transactions of those involving
    goods or services."),
    h2(br(), "Goods and Services"),
    djpr_plot_ui("total_bop_bar_chart",
      interactive = FALSE
    ),
    br(),
    djpr_plot_ui("good_services_export_chart"),
    br(),
    djpr_plot_ui("good_services_import_chart"),
    br(),
    djpr_plot_ui("goods_export_import_line"),
    br(),
    djpr_plot_ui("Vic_total_bop_bar_chart",
      interactive = FALSE
    ),
    h2(br(), "Goods"),
    djpr_plot_ui("goods_bop_bar_chart",
      interactive = FALSE
    ),
    br(),
    djpr_plot_ui("good_trade_line_chart"),
    br(),
    djpr_plot_ui("NSW_Vic_goods_line_chart"),
    br(),
    h2(br(), "Services"),
    djpr_plot_ui("service_bop_bar_chart",
      interactive = FALSE
    ),
    br(),
    djpr_plot_ui("services_trade_line_chart"),
    djpr_plot_ui("NSW_Vic_Services_line_chart"),
    br(),
    h2(br(), "Balance of Trade "),
    djpr_plot_ui("trade_balance_line_chart"),
    br(),
    centred_row(htmlOutput("bop_footnote")),
    br()
  )
}



page_bop <- function(input, output, session, plt_change, table_rowcount = 5){


    # Balance of Payments---
    # Goods and Services----
    djpr_plot_server("total_bop_bar_chart",
      viz_total_bop_bar_chart,
      data = bop,
      height_percent = 75,
      # width_percent = 75,
      plt_change = plt_change,
      date_slider = FALSE,
      interactive = FALSE
    )

    # Goods and Services: Goods and Services exports time series
    djpr_plot_server("good_services_export_chart",
      viz_good_services_export_chart,
      data = bop,
      plt_change = plt_change
    )

    # Goods and Services: Goods and Services imports time series

    djpr_plot_server("good_services_import_chart",
      viz_good_services_import_chart,
      data = bop,
      plt_change = plt_change
    )

    # Goods and Services: Annual growth in goods and services exports and imports
    djpr_plot_server("goods_export_import_line",
      viz_goods_export_import_line,
      data = bop,
      plt_change = plt_change
    )
    # Goods and Services: Export of goods and services for Victoria by calendar year
    djpr_plot_server("Vic_total_bop_bar_chart",
      viz_Vic_total_bop_bar_chart,
      data = bop,
      height_percent = 75,
      plt_change = plt_change,
      date_slider = FALSE,
      interactive = FALSE
    )
    # Balance of Payments---
    # Goods:Goods imports and exports since COVID
    djpr_plot_server("goods_bop_bar_chart",
      viz_goods_bop_bar_chart,
      data = bop,
      height_percent = 75,
      plt_change = plt_change,
      date_slider = FALSE,
      interactive = FALSE
    )
    # Goods: Goods imports and exports since covid
    djpr_plot_server("good_trade_line_chart",
      viz_good_trade_line_chart,
      data = bop %>%
        dplyr::filter(date >= as.Date("2018-12-01")),
      plt_change = plt_change
    )

    # Goods: Annual growth in goods exports and imports in NSW and Vic

    djpr_plot_server("NSW_Vic_goods_line_chart",
      viz_NSW_Vic_goods_line_chart,
      data = bop,
      plt_change = plt_change
    )

    # Balance of Payments---
    # Services: services imports and exports since COVID
    djpr_plot_server("service_bop_bar_chart",
      viz_service_bop_bar_chart,
      data = bop,
      height_percent = 75,
      plt_change = plt_change,
      date_slider = FALSE,
      interactive = FALSE
    )

    # Services: services imports and exports since COVID
    djpr_plot_server("services_trade_line_chart",
      viz_services_trade_line_chart,
      data = bop %>%
        dplyr::filter(date >= as.Date("2018-12-01")),
      plt_change = plt_change
    )

    # Services: Annual growth in services exports and imports in NSW and Vic

    djpr_plot_server("NSW_Vic_Services_line_chart",
      viz_NSW_Vic_Services_line_chart,
      data = bop,
      plt_change = plt_change
    )

    # Balance of trade:Cumulative change in total trade balance since December 2019
    djpr_plot_server("trade_balance_line_chart",
      viz_trade_balance_line_chart,
      data = bop %>%
        dplyr::filter(date >= as.Date("2018-12-01")),
      plt_change = plt_change
    )


}
