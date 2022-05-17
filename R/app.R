#' Shiny App
#'
#' @param ... unused
#'
#' @return
#' @export
#'
#' @examples
app <- function(...) {

  if(!exists("djpr_async_ui", envir = asNamespace("djprshiny"))) stop(
    "Please install latest djprshiny version:\n",
    "remotes::install_github(\"djpr-data/djprshiny\")"
  )

  trade_dash_cache <- cachem::cache_disk(
    dir = file.path(".", "app-cache")
  )

  shiny::shinyOptions(
    cache = trade_dash_cache
  )

  future::plan(future::sequential)


  assign(
    x = "con",
    value = pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = "opendata",
      host = '10.210.1.26',
      user = Sys.getenv()[['PG_READ_OPEN_USER']],
      password = Sys.getenv()[['PG_READ_OPEN_PW']],
      port = 443
    ),
    envir = .GlobalEnv
  )

  assign_table_global(
    con = con,
    tables = c(
      "merch",
      "merch_imp",
      "supp_cy",
      "supp_fy",
      "bop"
    )
  )

  merch %>%
    dplyr::summarise(
      min = min(date, na.rm = TRUE),
      max = max(date, na.rm = TRUE)
    ) %>%
    dplyr::collect()  %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.Date)) %>%
    assign("merch_dates", ., envir = .GlobalEnv)

  bop %>%
    dplyr::summarise(
      min = min(.data$date, na.rm = TRUE),
      max = max(.data$date, na.rm = TRUE)
    ) %>%
    dplyr::collect()  %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.Date)) %>%
    assign("bop_dates", ., envir = .GlobalEnv)

  merch %>%
    dplyr::group_by(sitc, sitc_code) %>%
    dplyr::summarize(n = length(sitc_code)) %>%
    dplyr::collect() %>%
    assign("merch_sitc_lu", ., envir = .GlobalEnv)

  merch %>%
    dplyr::summarize(sitc = dplyr::distinct(country_dest))  %>%
    dplyr::collect() %>%
    dplyr::pull() %>%
    assign("merch_country_dest", ., envir = .GlobalEnv)


  assign("in_global", ls(envir = .GlobalEnv), envir = .GlobalEnv)

  shiny::shinyApp(ui = ui(), server = server)
}
