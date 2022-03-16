page_launchpadUI <- function(id) {

  djprshiny::djpr_tab_panel(
    title = "Launchpad",
    djprshiny::ggiraph_js(),
    shiny::HTML(""),
    value = "tab-launchpad",
    shiny::br(),
    shiny::h1(
      span("DJPR Trade Dashboard",
           style = "font-size: 40px; color: #1F1547; font-family: 'Roboto Slab'"
      )
    ),
    shiny::br(),
    shiny::h2("Overview", align='center'),
    djpr_plot_ui("top_export_line_chart"),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(width = 6,
                    djpr_plot_ui("good_services_export_line_launchpad")
      ),
      shiny::column(width = 6,
                    djpr_plot_ui("top_country_line_chart")
      ),
    ),
    shiny::br(),
    shiny::h6("Below is a summary of high-level trade data, at both product (SITC) and country level. For more granular or specific data related to trade please use the other tabs in this app where appropriate." ),
    shiny::br(),
    shiny::br(),
    shiny::h2("Countries", align='center'),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::h4("Top 5 Exports ($m)"),
        shiny::uiOutput("country_export_table", height = "600px"),
        style='padding-left:0px; padding-right:20px;'),
      shiny::column(
        width = 6,
        shiny::h4("Top 5 Imports ($m)"),
        shiny::uiOutput("country_import_table", height = "600px"),
        style='padding-left:20px; padding-right:0px;')
    ),
    shiny::br(),
    shiny::h2("Products", align='center'),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::h4("Top 5 Exports ($m)"),
        shiny::uiOutput("product_export_table", height = "600px"),
        style='padding-left:0px; padding-right:20px;'),
      shiny::column(width = 6,
                    shiny::h4("Top 5 Imports ($m)"),
                    shiny::uiOutput("product_import_table", height = "600px"),
                    style='padding-left:20px; padding-right:0px;')
    ),
    shiny::br(),
    shiny::h2("Balance of Payments", align='center'),
    shiny::br(),
    shiny::uiOutput("launchpad_bop_table", height = "600px"),
    shiny::br(),
    djprshiny::centred_row(htmlOutput("launchpad_footnote")),
    shiny::br()

  )

}






page_launchpad <- function(input, output, session, plt_change, table_rowcount = 5){

  #Launchpad tables and charts
  djprshiny::djpr_plot_server("top_export_line_chart",
                   viz_launchpad_chart,
                   data = merch,
                   plt_change = plt_change,
                   date_slider_value_min = Sys.Date() - lubridate::years(2),
                   width_percent = 100,
                   convert_lazy = FALSE
  )


  djprshiny::djpr_plot_server("good_services_export_line_launchpad",
                              viz_good_services_export_chart,
                              data = bop,
                              plt_change = plt_change,
                              date_slider_value_min = Sys.Date() - lubridate::years(2),
                              width_percent = 50,
                              convert_lazy = FALSE
  )

  djprshiny::djpr_plot_server("top_country_line_chart",
                   viz_launchpad_countries,
                   data = merch,
                   plt_change = plt_change,
                   date_slider_value_min = Sys.Date() - lubridate::years(2),
                   width_percent = 50,
                   convert_lazy = FALSE
  )





  output$country_export_table <- renderUI({
    make_table_launchpad(data = tab_launchpad_country_imp_exp('export', merch, rows = table_rowcount)) %>%
      flextable::htmltools_value()
  })
  output$country_import_table <- renderUI({
    make_table_launchpad(data = tab_launchpad_country_imp_exp('import', merch_imp, rows = table_rowcount)) %>%
      flextable::htmltools_value()
  })
  output$product_export_table <- renderUI({
    make_table_launchpad(data = tab_launchpad_product_imp_exp('export', merch, rows = table_rowcount, sitc_level = 3)) %>%
      flextable::htmltools_value()
  })
  output$product_import_table <- renderUI({
    make_table_launchpad(data = tab_launchpad_product_imp_exp('import', merch_imp, rows = table_rowcount, sitc_level = 3)) %>%
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



}
