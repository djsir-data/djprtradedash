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

  # Page Footnotes

  footnote <- reactive({
    # req(dash_data)
    # latest <- max(series_latestdates)
    div(
      shiny::HTML(
        paste0(
          "This dashboard is produced by the <b>Strategy and Priority ",
          "Projects - Data + Analytics</b> team at the Victorian Department ",
          "of Jobs, Precincts and Regions. ", # The <b>latest data in this ",
          # "dashboard is for ",
          # format(latest, "%B %Y"),
          'Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard">email us</a> with any comments or feedback.'
        )
      ),
      style = "color: #828282; font-size: 0.75rem"
    )
  })

  output$launchpad_footnote <- output$bop_footnote <- output$merch_footnote <- output$country_footnote <- renderUI({
    footnote()
  })

  #Launchpad tables and charts
  djpr_plot_server("top_export_line_chart",
    viz_launchpad_chart,
    data = merch,
    plt_change = plt_change,
    date_slider_value_min = Sys.Date() - lubridate::years(3),
    width_percent = 100
    )

  # djpr_plot_server("top_export_line_chart",
  #   viz_launchpad_chart,
  #   data = merch,
  #   plt_change = plt_change,
  #   date_slider_value_min = Sys.Date() - lubridate::years(3),
  #   width_percent = 100,
  #   check_box_options = c(1,2,3),
  #   check_box_selected = 3,
  #   check_box_var = nchar(sitc_code),
  #   interactive = TRUE
  #   )

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
    make_table_launchpad(data = tab_launchpad_product_exp(rows = table_rowcount, sitc_level = 3)) %>%
      flextable::htmltools_value()
  })
  output$product_import_table <- renderUI({
    make_table_launchpad(data = tab_launchpad_product_imp(rows = table_rowcount, sitc_level = 3)) %>%
      flextable::htmltools_value()
  })
  output$launchpad_bop_table <- renderUI({
    make_table_launchpad(data = launchpad_table_export_import(),
                         header_row = c("",
                                      "Current figure ($m)",
                                      "Change since last quarter",
                                      "Change in past year",
                                      "Change since COVID")) %>%
      flextable::htmltools_value()
  })

  # Initialise country selection & URL query
  updateSelectInput(
    inputId = "country_select",
    choices = unique(merch$country_dest),
    selected = "China (excludes SARs and Taiwan)"
  )

  output$country_select <- renderText(
    paste0(
      "Victoria-",
      stringr::str_remove_all(input$country_select, " (.+)"),
      " Trade"
    )
  )

  merch_df <- shiny::reactive({
    if(input$merch_explorer_sitc %in% c(1,2,3)) {
      merch %>%
        dplyr::filter(nchar(.data$sitc_code) == input$merch_explorer_sitc) %>%
        dplyr::mutate(code_name = paste0(.data$sitc_code, ": ", .data$sitc))
    } else {
      merch %>%
        dplyr::mutate(code_name = paste0(.data$sitc_code, ": ", .data$sitc))
    }
  })

  observeEvent(merch_df(), {
    shinyWidgets::updateMultiInput(session = session, inputId = "merch_sitc", choices = unique(merch_df()$code_name))
  })

  merch_explorer_plot <- shiny::reactive({
    shiny::req(
      input$merch_explorer_dates,
      input$merch_countries,
      input$merch_sitc,
      input$merch_explorer_facets,
      input$merch_explorer_sitc
    )

    merch %>%
      dplyr::filter(
        .data$date >= input$merch_explorer_dates[1],
        .data$date <= input$merch_explorer_dates[2]
      ) %>%
      viz_merch_explorer(
        countries = input$merch_countries,
        goods = sub(".[0-9]*:\\s", "", input$merch_sitc),
        facet_by = input$merch_explorer_facets,
        smooth = input$merch_explorer_smooth
      )
  })

  output$merch_explorer <- shiny::renderPlot({
    merch_explorer_plot()
  })

  djprshiny::download_server(
    id = "merch_explorer_dl",
    plot = merch_explorer_plot(),
    plot_name = "merch_explorer_plot"
  )

  # Balance of Payments---
  # Goods and Services----
  output$table_export_import <- renderUI({
    table_export_import() %>%
      flextable::htmltools_value()
  })

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
  djpr_plot_server("vic_total_bop_cumul_line",
    viz_vic_total_bop_cumul_line,
    data = bop,
    height_percent = 75,
    plt_change = plt_change,
    date_slider = FALSE,
    interactive = TRUE
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

  # Country profiles
  # djpr_plot_server(
  #   "country_top_exp",
  #   viz_country_top_exp,
  #   data = merch,
  #   data_imp = merch_imp,
  #   plt_change = plt_change,
  #   country_select = reactive(input$country_select),
  #   date_slider_value_min = Sys.Date() - lubridate::years(3),
  #   height_percent = 160
  # )
  #
  # output$country_1y_exp_stat <- reactive({
  #   viz_country_1y_exp_stat(merch, input$country_select)
  # })
  #
  # output$country_1y_imp_stat <- reactive({
  #   viz_country_1y_imp_stat(merch_imp, input$country_select)
  # })
  #
  # output$country_1y_exp_change_stat <- reactive({
  #   viz_country_1y_exp_change_stat(merch, input$country_select)
  # })

}
