page_launchpad <- function(...) {
  djpr_tab_panel(
    title = "Launchpad",
    ggiraph_js(),
    HTML(""),
    value = "tab-launchpad",
    br(),
    br(),
    br(),
    br(),
    h1(
      span("DJPR Trade Dashboard",
        style = "font-size: 40px; color: #1F1547; font-family: 'Roboto Slab'"
      )
    ),
    br(),
    h2("Countries"),
    fluidRow(
      column(width = 6,
             h4("Top 10 Exports"),
             uiOutput("country_export_table", height = "600px"),
             style='padding-left:0px; padding-right:20px;'),
      column(width = 6,
             h4("Top 10 Imports"),
             uiOutput("country_import_table", height = "600px"),
             style='padding-left:20px; padding-right:0px;')
    ),
    br(),
    h2("Products"),
    fluidRow(column(width = 6,
                    h4("Top 10 Exports"),
                    uiOutput("product_export_table", height = "600px"),
                    style='padding-left:0px; padding-right:20px;'),
             column(width = 6,
                    h4("Top 10 Imports"),
                    uiOutput("product_import_table", height = "600px"),
                    style='padding-left:20px; padding-right:0px;')
    )

)

}
