page_country_profile <- function(...) {
  shiny::tabPanel(
    title =  "Country profiles",
    djprshiny::ggiraph_js(),
    value = "tab-country-profile",
    shiny::br(),
    djprshiny::centred_row(
      shiny::tagList(
        shiny::br(),
        shiny::h1(shiny::textOutput("country_select"), style = "text-align: center;"),
        shiny::h2(
          "Draft note: Data does not yet include services",
          style = "font-size: 130%;font-style: italic;color: #660000;text-align: center;"
        ),
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::span(
              shiny::textOutput("country_1y_exp_stat"),
              style = "font-size: 400%;text-align: center;font-weight: bold;"
              ),
            shiny::p(
              "12-month total exports",
              style = "text-align: center;font-style: italic;"
              )
            ),
          shiny::column(
            4,
            shiny::span(
              shiny::textOutput("country_1y_imp_stat"),
              style = "font-size: 400%;text-align: center;font-weight: bold;"
            ),
            shiny::p(
              "12-month total imports",
              style = "text-align: center;font-style: italic;"
            )
          ),
          shiny::column(
            4,
            shiny::span(
              shiny::textOutput("country_1y_exp_change_stat"),
              style = "font-size: 400%;text-align: center;font-weight: bold;"
            ),
            shiny::p(
              "Year on year export change",
              style = "text-align: center;font-style: italic;"
            )
          )
        ),
        shiny::br(),
        djprshiny::djpr_plot_ui("country_top_exp"),
        shiny::br()
      ),
      left_content =
        shiny::tagList(
          shiny::br(),
          shiny::br(),
          shiny::hr(),
          shiny::p(
            "Trading partner:",
            style = "font-size: 150%;font-style: italic;"
          ),
          shiny::selectInput("country_select", NULL, ""),
          shiny::actionButton("country_report", "Generate report"),
          shiny::hr(),
        )
    ),
    shiny::br(),
    djprshiny::centred_row(shiny::htmlOutput("country_footnote")),
    shiny::br()

  )
}
