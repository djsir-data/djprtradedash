#' Shiny App
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
app <- function(...) {
  trade_dash_cache <- cachem::cache_disk(
    dir = file.path(".", "app-cache")
  )

  shinyOptions(
    cache = trade_dash_cache
  )

  con <<- duckdb::dbConnect(drv = duckdb::duckdb(),
                            db = "trade_database.duckdb")

  merch <<- dplyr::tbl(con, 'merch') # collect all here is 6 sec
  merch_imp <<- dplyr::tbl(con, 'merch_imp')
  supp_cy <<- dplyr::tbl(con, 'supp_cy')
  supp_fy <<- dplyr::tbl(con, 'supp_fy')
  bop <<- dplyr::tbl(con, 'bop')

  merch_dates <<- merch %>%
    dplyr::summarise(
      min = min(date, na.rm = TRUE),
      max = max(date, na.rm = TRUE)
    ) %>%
    dplyr::collect()  %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.Date))

  bop_dates <<- bop %>%
    dplyr::summarise(
      min = min(.data$date, na.rm = TRUE),
      max = max(.data$date, na.rm = TRUE)
    ) %>%
    dplyr::collect()  %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.Date))

  merch_sitc_lu <<- merch  %>%
    dplyr::group_by(sitc, sitc_code) %>%
    dplyr::summarize(n = length(sitc_code)) %>%
    dplyr::collect()

  merch_country_dest <<- merch %>%
    dplyr::summarize(sitc = dplyr::distinct(country_dest))  %>%
    dplyr::collect() %>%
    dplyr::pull()



  in_global <<- ls(envir = .GlobalEnv)



  # close db connection on exit
  # onStop(function() {
  #   cat('Closing Pool')
  #   duckdb::dbDisconnect(con, shutdown = TRUE)
  #   rm(list = in_global, envir = .GlobalEnv)
  #   rm(in_global, envir = .GlobalEnv)
  # })


  shiny::shinyApp(ui = ui(), server = server)
}
