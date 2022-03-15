
ui <- function() {
  djprshiny::djpr_page(
    title = shiny::HTML("DJPR Trade<br>Dashboard"),
    page_launchpadUI('lp'),
    page_merchUI(),
    page_bopUI(),#,
    #page_country_profile()
    page_methodology()#,
    # page_country_profile()
  )
}
