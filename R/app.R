#' Shiny App
#'
#' @param ... unused
#'
#' @return
#' @export
#'
#' @examples
app <- function(...) {
  trade_dash_cache <- cachem::cache_disk(
    dir = file.path(".", "app-cache")
  )

  shiny::shinyOptions(
    cache = trade_dash_cache
  )

  assign(
    x = "con",
    value = duckdb::dbConnect(
      drv = duckdb::duckdb(read_only = TRUE, dbdir = normalizePath("./trade_database.duckdb")),
      db = "trade_database.duckdb"
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
