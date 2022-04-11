page_launchpadUI <- function(id) {

  shiny::fluidRow(
    djprshiny::djpr_h2_box("Overview"),
    djprshiny::djpr_plot_box(
      id = "top_export_line_chart",
      interactive = TRUE,
      width = 12
      ),
    djprshiny::djpr_plot_box(
      "good_services_export_line_launchpad",
      interactive = TRUE,
    ),
    djprshiny::djpr_plot_box(
      "top_country_line_chart",
      interactive = TRUE,
    ),
    shiny::br(),
    djprshiny::djpr_h2_box("Countries"),
    shinydashboard::box(
      title = shiny::h3("Top 5 Exports ($m)"),
      shiny::uiOutput("country_export_table", height = "600px")
    ),
    shinydashboard::box(
      title = shiny::h3("Top 5 Imports ($m)"),
      shiny::uiOutput("country_import_table", height = "600px")
    ),
    djprshiny::djpr_h2_box("Products"),
    shinydashboard::box(
      title = shiny::h3("Top 5 Exports ($m)"),
      shiny::uiOutput("product_export_table", height = "600px")
    ),
    shinydashboard::box(
      title = shiny::h3("Top 5 Imports ($m)"),
      shiny::uiOutput("product_import_table", height = "600px")
    ),
    djprshiny::djpr_h2_box("Balance of payments"),
    shinydashboard::box(
      shiny::uiOutput("launchpad_bop_table", height = "600px"),
      width = 12
    )
  )

}






page_launchpad <- function(input, output, session, plt_change, table_rowcount = 5){

  #Launchpad tables and charts
  djprshiny::djpr_plot_server(
    id                    = "top_export_line_chart",
    plot_function         = viz_launchpad_chart,
    data                  = merch,
    plt_change            = plt_change,
    date_slider_value_min = Sys.Date() - lubridate::years(2),
    width_percent         = 100,
    convert_lazy          = FALSE
  )


  djprshiny::djpr_plot_server(
    id                    = "good_services_export_line_launchpad",
    plot_function         = viz_good_services_export_chart,
    data                  = bop,
    plt_change            = plt_change,
    date_slider_value_min = Sys.Date() - lubridate::years(2),
    width_percent         = 50,
    convert_lazy          = FALSE
  )

  djprshiny::djpr_plot_server(
    id                    = "top_country_line_chart",
    plot_function         = viz_launchpad_countries,
    data                  = merch,
    plt_change            = plt_change,
    date_slider_value_min = Sys.Date() - lubridate::years(2),
    width_percent         = 50,
    convert_lazy          = FALSE
  )



  output$country_export_table <- shiny::renderUI({
    make_table_launchpad(
      data = tab_launchpad_country_imp_exp(
        direction = 'export',
        data      = merch,
        rows      = table_rowcount
        )
      ) %>%
      flextable::htmltools_value()
  })

  output$country_import_table <- shiny::renderUI({
    make_table_launchpad(
      data = tab_launchpad_country_imp_exp(
        direction = 'import',
        data      = merch_imp,
        rows      = table_rowcount
        )
      ) %>%
      flextable::htmltools_value()
  })

  output$product_export_table <- shiny::renderUI({
    make_table_launchpad(
      data = tab_launchpad_product_imp_exp(
        direction  = 'export',
        data       = merch,
        rows       = table_rowcount,
        sitc_level = 3
        )
      ) %>%
      flextable::htmltools_value()
  })

  output$product_import_table <- shiny::renderUI({
    make_table_launchpad(
      data = tab_launchpad_product_imp_exp(
        direction  = 'import',
        data       = merch_imp,
        rows       = table_rowcount,
        sitc_level = 3
        )
      ) %>%
      flextable::htmltools_value()
  })

  output$launchpad_bop_table <- shiny::renderUI({
    make_table_launchpad(
      data = launchpad_table_export_import(),
      header_row = c(
        "",
        "Current figure ($m)",
        "Change since last quarter",
        "Change in past year",
        "Change since COVID"
        )
      ) %>%
      flextable::htmltools_value()
  })



}
