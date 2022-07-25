
server <- function(input, output, session) {

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
      "service_trade",
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


  hchart_lang <- getOption("highcharter.lang")
  hchart_lang$numericSymbols <- c("k", "m", "b", "t", NULL, NULL)
  options(highcharter.lang = hchart_lang)

  page_launchpad(input, output, session, table_rowcount = 5)
  page_bop(input, output, session, plt_change, table_rowcount = 5)
  page_merch(input, output, session, plt_change)


  merch_last_12 <- merch_dates$max - months(12)

  sitc_merch <- merch %>%
    dplyr::filter(country_dest == "Total", date >= !!merch_last_12) %>%
    dplyr::group_by(sitc_code, sitc) %>%
    dplyr::summarise(sum_value = sum(value)) %>%
    dplyr::collect() %>%
    dplyr::mutate(sitc_level = as.character(nchar(sitc_code))) %>%
    dplyr::rename(
      `SITC Level` = sitc_level,
      `SITC Code` = sitc_code,
      `SITC Name` = sitc,
      `Total Exports in Last 12 Months ($000s)` = sum_value
      )

  output$sitc_table <- DT::renderDT(
    sitc_merch,
    filter = "top"
  )

}
