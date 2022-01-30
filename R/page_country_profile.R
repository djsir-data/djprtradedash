page_country_profile <- function(...) {
  tabPanel(
    title =  "Country profiles",
    ggiraph_js(),
    value = "tab-country-profile",
    br(),
    centred_row(
      tagList(
        br(),
        h1(textOutput("country_select"), style = "text-align: center;"),
        h2(
          "Draft note: Data does not yet include services",
          style = "font-size: 130%;font-style: italic;color: #660000;text-align: center;"
        ),
        br(),
        fluidRow(
          column(
            4,
            span(
              textOutput("country_1y_exp_stat"),
              style = "font-size: 400%;text-align: center;font-weight: bold;"
              ),
            p(
              "12-month total exports",
              style = "text-align: center;font-style: italic;"
              )
            ),
          column(
            4,
            span(
              textOutput("country_1y_imp_stat"),
              style = "font-size: 400%;text-align: center;font-weight: bold;"
            ),
            p(
              "12-month total imports",
              style = "text-align: center;font-style: italic;"
            )
          ),
          column(
            4,
            span(
              textOutput("country_1y_exp_change_stat"),
              style = "font-size: 400%;text-align: center;font-weight: bold;"
            ),
            p(
              "Year on year export change",
              style = "text-align: center;font-style: italic;"
            )
          )
        ),
        br(),
        djpr_plot_ui("country_top_exp")
      ),
      right_content =
        tagList(
          hr(),
          p(
            "Trading partner:",
            style = "font-size: 150%;font-style: italic;"
          ),
          selectInput("country_select", NULL, ""),
          span(
            actionButton("country_report", "Generate report"),
            style = "float: right;"
            ),
          hr(),
        )
    )

  )
}
