page_launchpadUI <- function(id) {

  shiny::tabPanel(
    title = "Launchpad",
    djprshiny::ggiraph_js(),
    useShinydashboard(),
    shiny::HTML(""),
    value = "tab-launchpad",
    shiny::fluidRow(shiny::column(5,
                                  style = 'padding-top:90px;',
                                  shinyWidgets::panel(
                                        style = 'height:400px;',
                                        heading = shiny::h1(style = 'text-align:center;',
                                          shiny::span(h1("DJPR Trade Dashboard"),
                                                                  style = "font-size: 50px;color:#1F1547;font-family:'vic-semibold' sans-serif;"
                                                      )),
                                        'add some text here')
                                        ),
                    shiny::column(7,
                          shiny::div(style = 'padding:20px;padding-top:90px;',
                            djprshiny::djpr_plot_ui("top_export_line_chart")
                          ))),
    shiny::fluidRow(shiny::column(4,
                    shinydashboard::box(
                      width = 12,
                      height = '200px',
                      background = 'blue',
                      solidHeader = TRUE,
                      title = 'Exports Explorer',
                      div(class = 'row',
                          div(class = 'col',
                              'Description of what to find in merch explorer',
                              div(style = 'position:absolute;bottom:20px',
                                  shinyWidgets::actionBttn('action1', "Explore",
                                                           style = 'material-flat',
                                                           color = 'default'))),
                          div(class = 'col-6',
                              style = "text-align: right;padding-bottom:15px;",
                              img(style = 'display:inline;height:10vh;width:auto;',
                                  src = 'https://icongr.am/material/binoculars.svg?size=164&color=fafafa',
                                  alt = 'small icon of binoculars')))

                    )),
                    shiny::column(4,
                    shinydashboard::box(
                      width = 12,
                      height = '200px',
                      background = 'blue',
                      solidHeader = TRUE,
                      title = "Balance of Payments",
                      div(class = 'row',
                          div(class = 'col',
                              'description of balance of payments',
                              div(style = 'position:absolute;bottom:20px',
                                  shinyWidgets::actionBttn('action2', "Balance",
                                                           style = 'material-flat',
                                                           color = 'default'))),
                          div(class = 'col-6',
                              style = "text-align: right;padding-bottom:15px;",
                              img(style = 'display:inline;height:10vh;width:auto;',
                                  src = 'https://icongr.am/clarity/balance.svg?size=164&color=fafafa',
                                  alt = 'small icon of weighing balance')))

                    )),
                    shiny::column(4,
                    shinydashboard::box(
                      width = 12,
                      height = '200px',
                      background = 'blue',
                      solidHeader = TRUE,
                      title = "Help and Methodology",
                      div(class = 'row',
                          div(class = 'col',
                              'some text here',
                              div(style = 'position:absolute;bottom:20px',
                                  shinyWidgets::actionBttn('action3', 'Help',
                                                           style = 'material-flat',
                                                           color = 'default'))),
                          div(class="col-6",
                              style = "text-align: right;padding-bottom:15px;",
                              img(style = 'display:inline;height:10vh;width:auto;',
                                  src = 'https://icongr.am/clarity/help-info.svg?color=fafafa',
                                  alt = 'small icon of I for information')))

                    ))),

    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::div(style = 'padding:20px;display: inline-block;',
          djprshiny::djpr_plot_ui("good_services_export_line_launchpad", width = '40%')
        )
      ),
      shiny::column(
        width = 6,
        shiny::div(style = 'padding:20px;display: inline-block;',
          djprshiny::djpr_plot_ui("top_country_line_chart", width = '40%')
        )
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
      shiny::column(
        width = 6,
        shiny::h4("Top 5 Imports ($m)"),
        shiny::uiOutput("product_import_table", height = "600px"),
        style='padding-left:20px; padding-right:0px;')
    ),
    shiny::br(),
    shiny::h2("Balance of Payments", align='center'),
    shiny::br(),
    shiny::uiOutput("launchpad_bop_table", height = "600px"),
    shiny::br(),
    djprshiny::centred_row(shiny::htmlOutput("launchpad_footnote")),
    shiny::br()

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
    width_percent         = 80,
    convert_lazy          = FALSE
  )

  djprshiny::djpr_plot_server(
    id                    = "top_country_line_chart",
    plot_function         = viz_launchpad_countries,
    data                  = merch,
    plt_change            = plt_change,
    date_slider_value_min = Sys.Date() - lubridate::years(2),
    width_percent         = 80,
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
