page_launchpadUI <- function(id) {


  shiny::tagList(
      shiny::fluidRow(
        djprshiny::djpr_h2_box("DJPR Trade Dashboard")),
      shiny::fluidRow(
        column(5,
               shinyWidgets::panel(
                 style = 'height:530px;',
                 'add some text here')),
        column(7,
               djprshiny::djpr_plot_box(
                 id = "top_export_line_chart",
                 interactive = TRUE,
                 width = 12
               ))
      ),

      shiny::fluidRow(style = 'padding:20px;',
        shiny::column(4,

                      div(class = 'card text-white bg-dark mb-3',
                          style = 'height:200px;',
                          div(class = 'card-header',
                              h4('Exports Explorer')
                          ),
                          div(class = 'card-body',
                              div(class = 'row',
                                  div(class = 'col',
                                      'Description of what to find in merch explorer',
                                      div(style = 'position:absolute;bottom:20px',
                                          shinyWidgets::actionBttn('btn_explore', "Explore",
                                                                   style = 'material-flat',
                                                                   color = 'success'))),
                                  div(class = 'col-6',
                                      style = "text-align: right;padding-bottom:15px;",
                                      img(style = 'display:inline;height:10vh;width:auto;',
                                          src = 'https://icongr.am/material/binoculars.svg?size=164&color=fafafa',
                                          alt = 'small icon of binoculars')))
                          )
                      )
      ),
      shiny::column(4,

                    div(class = 'card text-white bg-dark mb-3',
                        style = 'height:200px;',
                        div(class = 'card-header',
                            h4("Balance of Payments")),
                        div(class = 'card-body',
                            div(class = 'row',
                                div(class = 'col',
                                    'description of balance of payments',
                                    div(style = 'position:absolute;bottom:20px',
                                        shinyWidgets::actionBttn('btn_balance', "Balance",
                                                                 style = 'material-flat',
                                                                 color = 'success'))),
                                div(class = 'col-6',
                                    style = "text-align: right;padding-bottom:15px;",
                                    img(style = 'display:inline;height:10vh;width:auto;',
                                        src = 'https://icongr.am/clarity/balance.svg?size=164&color=fafafa',
                                        alt = 'small icon of weighing balance')))
                        )
                    )
      ),
      shiny::column(4,

                    div(class = 'card text-white bg-dark mb-3',
                        style = 'height:200px;',
                        div(class = 'card-header',
                            h4("Help and Methodology")),
                        div(class = 'card-body',
                            div(class = 'row',
                                div(class = 'col',
                                    'some text here',
                                    div(style = 'position:absolute;bottom:20px',
                                        shinyWidgets::actionBttn('btn_help', 'Help',
                                                                 style = 'material-flat',
                                                                 color = 'success'))),
                                div(class="col-6",
                                    style = "text-align: right;padding-bottom:15px;",
                                    img(style = 'display:inline;height:10vh;width:auto;',
                                        src = 'https://icongr.am/clarity/help-info.svg?color=fafafa',
                                        alt = 'small icon of I for')))
                        )
                    )
      )),
    fluidRow(

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
        title = h3("Top 5 Exports ($m)"),
        shiny::uiOutput("country_export_table", height = "600px")
      ),
      shinydashboard::box(
        title = h3("Top 5 Imports ($m)"),
        shiny::uiOutput("country_import_table", height = "600px")
      ),
      djprshiny::djpr_h2_box("Products"),
      shinydashboard::box(
        title = h3("Top 5 Exports ($m)"),
        shiny::uiOutput("product_export_table", height = "600px")
      ),
      shinydashboard::box(
        title = h3("Top 5 Imports ($m)"),
        shiny::uiOutput("product_import_table", height = "600px")
      ),
      djprshiny::djpr_h2_box("Balance of payments"),
      shinydashboard::box(
        shiny::uiOutput("launchpad_bop_table", height = "600px"),
        width = 12
      )

    )

  )





}






page_launchpad <- function(input, output, session, plt_change, table_rowcount = 5){


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
