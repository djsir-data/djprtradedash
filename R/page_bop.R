page_bopUI <- function(...) {

  shiny::tagList(

    # Overview
    djprshiny::djpr_h2_box("Balance of Payments"),

    djprshiny::djpr_async_ui(
      id = "good_services_chart",
      width = 12,
      bop_date_slider("good_services_chart", width = "50%")
    ),

    djprshiny::djpr_async_ui(
      "goods_export_import_line",
      bop_date_slider(
        "goods_export_import_line",
        value = c(
          max(bop_dates$min, bop_dates$max - lubridate::years(5)),
          bop_dates$max
        )
      )
    ),

    djprshiny::djpr_async_ui("total_bop_bar_chart"),


    # Goods
    djprshiny::djpr_h2_box("Goods"),

    djprshiny::djpr_async_ui("goods_bop_bar_chart"),

    djprshiny::djpr_async_ui(
      "good_trade_line_chart",
      bop_date_slider(
        "good_trade_line_chart",
        min = as.Date("2018-12-01"),
        value = c(as.Date("2018-12-01"), bop_dates$max)
      )
    ),

    djprshiny::djpr_async_ui(
      "NSW_Vic_goods_line_chart",
      width = 12,
      bop_date_slider("NSW_Vic_goods_line_chart")
    ),

    # Services
    djprshiny::djpr_h2_box("Services"),

    djprshiny::djpr_async_ui("service_bop_bar_chart"),

    djprshiny::djpr_async_ui(
      "services_trade_line_chart",
      bop_date_slider(
        "services_trade_line_chart",
        min = as.Date("2018-12-01"),
        value = c(as.Date("2018-12-01"), bop_dates$max)
      )
    ),

    djprshiny::djpr_async_ui(
      "NSW_Vic_Services_line_chart",
      width = 12,
      bop_date_slider("NSW_Vic_Services_line_chart")
      ),

    # Balance of trade
    djprshiny::djpr_h2_box("Balance of Trade"),
    djprshiny::djpr_async_ui(
      id = "trade_balance_line_chart",
      width = 12,
      bop_date_slider("trade_balance_line_chart")
    )
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
    plt_change    = plt_change,
    convert_lazy   = FALSE
  )

  # Totals imports and exports since COVID
  djprshiny::djpr_async_server(
    id       = "total_bop_bar_chart",
    plot_fun = viz_total_bop_bar_chart,
    data     = bop
  )

  # Goods and Services: Goods and Services imports time series

  djprshiny::djpr_async_server(
    id         = "good_services_chart",
    plot_fun   = viz_good_services_chart,
    dates      = input$dates,
    facet_cols = input$sizing$width > input$sizing$height
  )

  # Goods and Services: Annual growth in goods and services exports and imports
  djprshiny::djpr_async_server(
    id       = "goods_export_import_line",
    plot_fun = viz_goods_export_import_line,
    data     = bop,
    dates    = input$dates
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
  djprshiny::djpr_async_server(
    id       = "goods_bop_bar_chart",
    plot_fun = viz_goods_bop_bar_chart,
    data     = bop
  )
  # Goods: Goods imports and exports since covid
  djprshiny::djpr_async_server(
    id       = "good_trade_line_chart",
    plot_fun = viz_good_trade_line_chart,
    dates    = input$dates
  )

  # Goods: Annual growth in goods exports and imports in NSW and Vic

  djprshiny::djpr_async_server(
    id       = "NSW_Vic_goods_line_chart",
    plot_fun = viz_NSW_Vic_goods_line_chart,
    data     = bop
  )

  # Balance of Payments---
  # Services: services imports and exports since COVID
  djprshiny::djpr_async_server(
    id       = "service_bop_bar_chart",
    plot_fun = viz_service_bop_bar_chart,
    data     = bop
  )

  # Services: services imports and exports since COVID
  djprshiny::djpr_async_server(
    id       = "services_trade_line_chart",
    plot_fun = viz_services_trade_line_chart,
    data     = bop,
    dates    = input$dates
  )

  # Services: Annual growth in services exports and imports in NSW and Vic
  djprshiny::djpr_async_server(
    id       = "NSW_Vic_Services_line_chart",
    plot_fun = viz_NSW_Vic_Services_line_chart,
    data     = bop,
    dates    = input$dates
  )

  # Balance of trade:Cumulative change in total trade balance since December 2019
  djprshiny::djpr_async_server(
    id       = "trade_balance_line_chart",
    plot_fun = viz_trade_balance_line_chart,
    data     = bop,
    dates    = input$dates
  )


}
