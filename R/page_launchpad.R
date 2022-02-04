page_launchpad <- function(...) {
  djpr_tab_panel(
    title = "Launchpad",
    ggiraph_js(),
    HTML(""),
    value = "tab-launchpad",
    br(),
    h1(
      span("DJPR Trade Dashboard",
        style = "font-size: 40px; color: #1F1547; font-family: 'Roboto Slab'"
      )
    ),
    djpr_plot_ui("top_export_line_chart"),
    br(),
    fluidRow(
      column(width = 6,
        djpr_plot_ui("good_services_export_line_launchpad")
        ),
      column(width = 6,
        djpr_plot_ui("top_country_line_chart")
        ),
      ),
    br(),
    h6("Below is a summary of high-level trade data, at both product (SITC) and country level. For more granular or specific data related to trade please use the search box found below." ),
    br(),
    # shinyWidget::searchInput(
    #   inputId = "search", label = "Enter your text",
    #   placeholder = "A placeholder",
    #   btnSearch = icon("search"),
    #   btnReset = icon("remove"),
    #   width = "450px"
    # ),
    br(),
    h2("Countries", align='center'),
    br(),
    fluidRow(
      column(width = 6,
             h4("Top 5 Exports"),
             uiOutput("country_export_table", height = "600px"),
             style='padding-left:0px; padding-right:20px;'),
      column(width = 6,
             h4("Top 5 Imports"),
             uiOutput("country_import_table", height = "600px"),
             style='padding-left:20px; padding-right:0px;')
    ),
    br(),
    h2("Products", align='center'),
    br(),
    fluidRow(column(width = 6,
                    h4("Top 5 Exports"),
                    uiOutput("product_export_table", height = "600px"),
                    style='padding-left:0px; padding-right:20px;'),
             column(width = 6,
                    h4("Top 5 Imports"),
                    uiOutput("product_import_table", height = "600px"),
                    style='padding-left:20px; padding-right:0px;')
    )

)

}
