
ui <- function() {
  djprshiny::djpr_page(
    title = shiny::HTML("DJPR Trade<br>Dashboard"),
    page_launchpadUI('lp') #,
    #page_merch(),
    #page_bop(),
    #page_country_profile()
  )
}
