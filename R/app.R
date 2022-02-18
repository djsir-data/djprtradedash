app <- function(...) {
  trade_dash_cache <- cachem::cache_disk(
    dir = file.path(".", "app-cache")
  )

  shinyOptions(
    cache = trade_dash_cache
  )

  # close db connection on exit
  onStop(function() {
    cat('Closing Pool')
    pool::poolClose(db_pool)
  })


  shiny::shinyApp(ui(), server)
}
