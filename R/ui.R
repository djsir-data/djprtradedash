
ui <- function() {
  djprshiny::djpr_page(
    title = shiny::HTML("DJPR Trade<br>Dashboard"),
    page_launchpad(),
    page_merch()
  )
}
