page_country_profile <- function(...) {
  tabPanel(
    title =  "Country profiles",
    ggiraph_js(),
    value = "tab-country-profile",
    br(),
    centred_row(
      selectInput("country_select", "Country", ""),
      h1(textOutput("country_select")),
      p("to be completed"),
      djpr_plot_ui("country_top_exp")
    )

  )
}
