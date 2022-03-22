page_bopUI <- function(...) {
  djprshiny::djpr_tab_panel(
    title = "Balance of Payments",
    shiny::h1(
      shiny::span("Balance of payments",
           style = "font-size: 40px; color: #1F1547; font-family: 'Roboto Slab'"
      )
    ),
    shiny::p(
      "Summary information on Victoria's quarterly trade",
      style = "font-size: 150%;font-style: italic;color: #A9A9A9"
      ),
    shiny::br(),
    shiny::br(),
    shiny::h2("Goods and Services", align = "center"),
    shiny::br(),
    djprshiny::djpr_plot_ui("good_services_chart"),
    shiny::br(),
    djprshiny::djpr_plot_ui("total_bop_bar_chart",
      interactive = FALSE
    ),
    shiny::br(),
    djprshiny::djpr_plot_ui("goods_export_import_line"),
    shiny::br(),
    djprshiny::djpr_plot_ui("vic_total_bop_cumul_line"),
    shiny::br(),
    shiny::br(),
    shiny::h2("Goods", align = "center"),
    shiny::br(),
    djprshiny::djpr_plot_ui("goods_bop_bar_chart",
      interactive = FALSE
    ),
    shiny::br(),
    djprshiny::djpr_plot_ui("good_trade_line_chart"),
    shiny::br(),
    djprshiny::djpr_plot_ui("NSW_Vic_goods_line_chart"),
    shiny::br(),
    shiny::br(),
    shiny::h2("Services", align = "center"),
    shiny::br(),
    djprshiny::djpr_plot_ui("service_bop_bar_chart",
      interactive = FALSE
    ),
    shiny::br(),
    djprshiny::djpr_plot_ui("services_trade_line_chart"),
    djprshiny::djpr_plot_ui("NSW_Vic_Services_line_chart"),
    shiny::br(),
    shiny::br(),
    shiny::h2("Balance of Trade", align = "center"),
    shiny::br(),
    djprshiny::djpr_plot_ui("trade_balance_line_chart")
  )
}



page_bop <- function(input, output, session, plt_change, table_rowcount = 5){


  # Balance of Payments---
  # Goods and Services----
  output$table_export_import <- shiny::renderUI({
    table_export_import() %>%
      flextable::htmltools_value()
  })



  # Goods and Services: Goods and Services exports time series
  djprshiny::djpr_plot_server(
    id            = "good_services_export_chart",
    plot_function = viz_good_services_export_chart,
    data          = bop,
    plt_change    = plt_change
  )

  # Totals imports and exports since COVID
  djprshiny::djpr_plot_server(
    id             = "total_bop_bar_chart",
    plot_function  = viz_total_bop_bar_chart,
    data           = bop,
    height_percent = 75,
    plt_change     = plt_change,
    date_slider    = FALSE,
    interactive    = FALSE
  )

  # Goods and Services: Goods and Services imports time series

  djprshiny::djpr_plot_server(
    id                    = "good_services_chart",
    plot_function         = viz_good_services_chart,
    data                  = bop,
    plt_change            = plt_change,
    date_slider_value_min = bop_dates$max - lubridate::years(5)
  )

  # Goods and Services: Annual growth in goods and services exports and imports
  djprshiny::djpr_plot_server(
    id            = "goods_export_import_line",
    plot_function = viz_goods_export_import_line,
    data          = bop,
    plt_change    = plt_change
  )
  # Goods and Services: Export of goods and services for Victoria by calendar year
  djprshiny::djpr_plot_server(
    id             = "vic_total_bop_cumul_line",
    plot_function  = viz_vic_total_bop_cumul_line,
    data           = bop,
    bop_dates      = bop_dates,
    height_percent = 75,
    plt_change     = plt_change,
    date_slider    = FALSE,
    convert_lazy   = FALSE
  )
  # Balance of Payments---
  # Goods:Goods imports and exports since COVID
  djprshiny::djpr_plot_server(
    id             = "goods_bop_bar_chart",
    plot_function  = viz_goods_bop_bar_chart,
    data           = bop,
    height_percent = 75,
    plt_change     = plt_change,
    date_slider    = FALSE,
    interactive    = FALSE
  )
  # Goods: Goods imports and exports since covid
  djprshiny::djpr_plot_server(
    id            = "good_trade_line_chart",
    plot_function = viz_good_trade_line_chart,
    data          = dplyr::filter(bop, date >= as.Date("2018-12-01")),
    plt_change    = plt_change
  )

  # Goods: Annual growth in goods exports and imports in NSW and Vic

  djprshiny::djpr_plot_server(
    id            = "NSW_Vic_goods_line_chart",
    plot_function = viz_NSW_Vic_goods_line_chart,
    data          = bop,
    plt_change    = plt_change
  )

  # Balance of Payments---
  # Services: services imports and exports since COVID
  djprshiny::djpr_plot_server(
    id             = "service_bop_bar_chart",
    plot_function  = viz_service_bop_bar_chart,
    data           = bop,
    height_percent = 75,
    plt_change     = plt_change,
    date_slider    = FALSE,
    interactive    = FALSE
  )

  # Services: services imports and exports since COVID
  djprshiny::djpr_plot_server(
    id            = "services_trade_line_chart",
    plot_function = viz_services_trade_line_chart,
    data          = dplyr::filter(bop, date >= as.Date("2018-12-01")),
    plt_change    = plt_change
  )

  # Services: Annual growth in services exports and imports in NSW and Vic

  djprshiny::djpr_plot_server(
    id            = "NSW_Vic_Services_line_chart",
    plot_function = viz_NSW_Vic_Services_line_chart,
    data          = bop,
    plt_change    = plt_change
  )

  # Balance of trade:Cumulative change in total trade balance since December 2019
  djprshiny::djpr_plot_server(
    id            = "trade_balance_line_chart",
    plot_function = viz_trade_balance_line_chart,
    data          = dplyr::filter(bop, date >= as.Date("2018-12-01")),
    plt_change    = plt_change
  )


}
