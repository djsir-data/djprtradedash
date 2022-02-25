
ui <- function() {
  djprshiny::djpr_page(
    title = shiny::HTML("DJPR Trade<br>Dashboard"),
    page_launchpadUI('lp'),
    page_merch(),
    page_bopUI()#,
    #page_country_profile()
  )
}
