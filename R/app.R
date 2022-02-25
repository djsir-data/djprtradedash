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
    duckdb::dbDisconnect(con, shutdown = TRUE)
    rm(list = in_global, envir = .GlobalEnv)
    rm(in_global, envir = .GlobalEnv)
  })


  shiny::shinyApp(ui(), server)
}
