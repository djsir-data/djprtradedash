#' @import djprshiny
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import djprtheme
#' @importFrom rlang .data .env

server <- function(input, output, session) {
  plt_change <- reactive(input$plt_change) %>%
    debounce(2)

  #Launchpad searchbar
  server <- function(input, output, session) {
    output$res <- renderPrint({
      input$search
    })
  }

  #Launchpad tables and charts
  djpr_plot_server("top_export_line_chart",
    viz_launchpad_chart,
    data = merch,
    plt_change = plt_change,
    date_slider_value_min = Sys.Date() - lubridate::years(3),
    width_percent = 100
    )

  djpr_plot_server("top_country_line_chart",
    viz_launchpad_countries,
    data = merch,
    plt_change = plt_change,
    date_slider_value_min = Sys.Date() - lubridate::years(3),
    width_percent = 50
    )

  djpr_plot_server("good_services_export_line_launchpad",
    viz_good_services_import_chart,
    data = bop,
    plt_change = plt_change,
    width_percent = 50
  )

  table_rowcount <- 5

  output$country_export_table <- renderUI({
    make_table_launchpad(data = tab_launchpad_country_exp(rows = table_rowcount)) %>%
      flextable::htmltools_value()
  })
  output$country_import_table <- renderUI({
    make_table_launchpad(data = tab_launchpad_country_imp(rows = table_rowcount)) %>%
      flextable::htmltools_value()
  })
  output$product_export_table <- renderUI({
    make_table_launchpad(data = tab_launchpad_product_exp(rows = table_rowcount, sitc_level = 1)) %>%
      flextable::htmltools_value()
  })
  output$product_import_table <- renderUI({
    make_table_launchpad(data = tab_launchpad_product_imp(rows = table_rowcount, sitc_level = 1)) %>%
      flextable::htmltools_value()
  })


  # djpr_plot_server(
  #   "merch_explorer",
  #   viz_merch_explorer,
  #   data = merch,
  #   countries = reactive(input$merch_countries),
  #   goods = reactive(input$merch_sitc),
  #   plt_change = plt_change
  # )

  merch_explorer_plot <- reactive({
    req(
      input$merch_explorer_dates,
      input$merch_countries,
      input$merch_sitc,
      input$merch_explorer_facets
    )

    merch %>%
      dplyr::filter(
        .data$date >= input$merch_explorer_dates[1],
        .data$date <= input$merch_explorer_dates[2]
      ) %>%
      viz_merch_explorer(
        countries = input$merch_countries,
        goods = input$merch_sitc,
        facet_by = input$merch_explorer_facets,
        smooth = input$merch_explorer_smooth
      )
  })

  output$merch_explorer <- renderPlot({
    merch_explorer_plot()
  })

  djprshiny::download_server(
    id = "merch_explorer_dl",
    plot = merch_explorer_plot(),
    plot_name = "merch_explorer_plot"
  )

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
