app <- function(...) {
  trade_dash_cache <- cachem::cache_disk(
    dir = file.path(".", "app-cache")
  )

  shinyOptions(
    cache = trade_dash_cache
  )

  shiny::shinyApp(ui(), server, enableBookmarking = "url")
}
