page_launchpadUI <- function(id) {


  shiny::tagList(

      # Launchpad text & export plot
      djprshiny::djpr_h2_box("DJPR Trade Dashboard"),
      shiny::column(
        5,
        shiny::div(
          class = "box",
          style = "height: 450px;",
          shiny::p(
            style = "padding: 10px;",
            "Conthent goes here"
            )
          )
        ),
      djprshiny::djpr_async_ui(
        id    = "top_export_line_chart",
        width = 7,
        height = "400px",
        merch_date_slider("top_export_line_chart")
      ),

      # Cards
      shiny::fluidRow(style = 'padding:20px;',
        shiny::column(4,

                      shiny::div(class = 'card text-white bg-dark mb-3',
                          style = 'height:200px;',
                          shiny::div(class = 'card-header',
                                     shiny::h4('Exports Explorer')
                          ),
                          shiny::div(class = 'card-body',
                                     shiny::div(class = 'row',
                                                shiny::div(class = 'col',
                                      'Description of what to find in merch explorer',
                                      shiny::div(style = 'position:absolute;bottom:20px',
                                          shinyWidgets::actionBttn('btn_explore', "Explore",
                                                                   style = 'material-flat',
                                                                   color = 'success'))),
                                      shiny::div(class = 'col-6',
                                      style = "text-align: right;padding-bottom:15px;",
                                      shiny::img(style = 'display:inline;height:10vh;width:auto;',
                                          src = 'https://icongr.am/material/binoculars.svg?size=164&color=fafafa',
                                          alt = 'small icon of binoculars')))
                          )
                      )
      ),
      shiny::column(4,

                    shiny::div(class = 'card text-white bg-dark mb-3',
                        style = 'height:200px;',
                        shiny::div(class = 'card-header',
                                   shiny::h4("Balance of Payments")),
                        shiny:: div(class = 'card-body',
                                    shiny::div(class = 'row',
                                               shiny::div(class = 'col',
                                    'description of balance of payments',
                                    shiny::div(style = 'position:absolute;bottom:20px',
                                        shinyWidgets::actionBttn('btn_balance', "Balance",
                                                                 style = 'material-flat',
                                                                 color = 'success'))),
                                    shiny::div(class = 'col-6',
                                    style = "text-align: right;padding-bottom:15px;",
                                    shiny::img(style = 'display:inline;height:10vh;width:auto;',
                                        src = 'https://icongr.am/clarity/balance.svg?size=164&color=fafafa',
                                        alt = 'small icon of weighing balance')))
                        )
                    )
      ),
      shiny::column(4,

                    shiny::div(class = 'card text-white bg-dark mb-3',
                        style = 'height:200px;',
                        shiny::div(class = 'card-header',
                                   shiny::h4("Help and Methodology")),
                        shiny::div(class = 'card-body',
                                   shiny::div(class = 'row',
                                              shiny::div(class = 'col',
                                    'some text here',
                                    shiny::div(style = 'position:absolute;bottom:20px',
                                        shinyWidgets::actionBttn('btn_help', 'Help',
                                                                 style = 'material-flat',
                                                                 color = 'success'))),
                                    shiny::div(class="col-6",
                                    style = "text-align: right;padding-bottom:15px;",
                                    shiny::img(style = 'display:inline;height:10vh;width:auto;',
                                        src = 'https://icongr.am/clarity/help-info.svg?color=fafafa',
                                        alt = 'small icon of I for')))
                        )
                    )
      )),


      # Line charts
      djprshiny::djpr_async_ui(
        "good_services_export_line_launchpad",
        height = "400px",
        merch_date_slider("good_services_export_line_launchpad")
      ),

      djprshiny::djpr_async_ui(
        "top_country_line_chart",
        height = "400px",
        merch_date_slider("top_country_line_chart")
      ),

      # Country tables
      djprshiny::djpr_h2_box("Countries"),
      shinydashboard::box(
        title = shiny::h3("Top 5 Exports ($m)"),
        shiny::uiOutput("country_export_table", height = "600px")
      ),
      shinydashboard::box(
        title = shiny::h3("Top 5 Imports ($m)"),
        shiny::uiOutput("country_import_table", height = "600px")
      ),

      # Product tables
      djprshiny::djpr_h2_box("Products"),
      shinydashboard::box(
        title = shiny::h3("Top 5 Exports ($m)"),
        shiny::uiOutput("product_export_table", height = "600px")
      ),
      shinydashboard::box(
        title = shiny::h3("Top 5 Imports ($m)"),
        shiny::uiOutput("product_import_table", height = "600px")
      ),

      # BOP table
      djprshiny::djpr_h2_box("Balance of payments"),
      shinydashboard::box(
        shiny::uiOutput("launchpad_bop_table", height = "600px"),
        width = 12
      )

  )
}






page_launchpad <- function(input, output, session, table_rowcount = 5){


  # info nav buttons
  observeEvent(input$btn_explore, {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "merch")
  })
  observeEvent(input$btn_balance, {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "bop")
  })
  observeEvent(input$btn_help, {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "methodology")
  })


  #Launchpad tables and charts
  djprshiny::djpr_async_server(
    id       = "top_export_line_chart",
    plot_fun = viz_launchpad_chart,
    data     = merch,
    dates    = input$dates
  )

  djprshiny::djpr_async_server(
    id       = "good_services_export_line_launchpad",
    plot_fun = viz_good_services_export_chart,
    data     = bop,
    dates    = input$dates
  )

  djprshiny::djpr_async_server(
    id       = "top_country_line_chart",
    plot_fun = viz_launchpad_countries,
    data     = merch,
    dates    = input$dates
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
