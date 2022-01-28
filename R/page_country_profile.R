page_country_profile <- function(...) {
  tabPanel(
    title =  "Country profiles",
    ggiraph_js(),
    value = "tab-country-profile",
    br(),
    centred_row(
      tagList(
        br(),
        selectInput("country_select", "Country", ""),
        br(),
        h1(textOutput("country_select")),
        br(),
        fluidRow(
          column(
            4,
            p("99%", style = "font-size: 400%;text-align: center;font-weight: bold;"),
            p(
              "Year on year export change",
              style = "text-align: center;font-style: italic;"
              )
            ),
          column(
            4,
            p("99%", style = "font-size: 400%;text-align: center;font-weight: bold;"),
            p(
              "Year on year export change",
              style = "text-align: center;font-style: italic;"
            )
          ),
          column(
            4,
            p("99%", style = "font-size: 400%;text-align: center;font-weight: bold;"),
            p(
              "Year on year export change",
              style = "text-align: center;font-style: italic;"
            )
          )
        ),
        br(),
        djpr_plot_ui("country_top_exp")
      )
    )

  )
}
