#' Shiny App
#'
#' @param ... unused
#'
#' @return
#' @export

app <- function(...) {

  trade_dash_cache <- cachem::cache_disk(
    dir = file.path(".", "app-cache")
  )

  shiny::shinyOptions(
    cache = trade_dash_cache
  )

  load_tabs()

  shiny::shinyApp(ui = ui(), server = server)
}
