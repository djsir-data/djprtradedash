page_launchpad <- function(...) {
  tabPanel(
    title = "Launchpad",
    ggiraph_js(),
    HTML(""),
    value = "tab-launchpad",
    br(),
    br(),
    br(),
    br(),
    centred_row(
      span("DJPR Trade Dashboard",
           style = "font-size: 40px; color: #1F1547; font-family: 'Roboto Slab'"
      )
    ),
    br(),
    centred_row("Content goes here")
  )
}
