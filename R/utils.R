# Transpose and string split factor levels
tstrsplit_factor <- function(fac, split){
  lev <- levels(fac)
  ind <- as.integer(fac)
  split <- data.table::tstrsplit(lev, split = split)
  lapply(split, function(x) x[ind])
}


# Trycatch infix function
`%iferror%` <- function(a, b) tryCatch({a}, error = function(e){b})


# Assign multiple lazy data frames to global environement
assign_table_global <- function(con, tables){
  lapply(tables, function(tab){
    assign(tab, dplyr::tbl(con, tab), envir = .GlobalEnv)
  })
}


# Format dollar figures
dollar_stat <- function(stat){
  dplyr::case_when(
    stat > 1e10 ~ scales::dollar(
      stat / 1e09,
      suffix = "b"
    ),
    stat > 1e09 ~ scales::dollar(
      stat / 1e09,
      suffix = "b"
    ),
    stat > 1e07 ~ scales::dollar(
      stat / 1e06,
      prefix = "$",
      suffix = "m"
    ),
    stat > 1e06 ~ scales::dollar(
      stat / 1e06,
      prefix = "$",
      suffix = "m"
    ),
    stat > 1e04 ~ scales::dollar(
      stat / 1e03,
      suffix = "k"
    ),
    stat > 1e03 ~ scales::dollar(
      stat / 1e03,
      suffix = "k"
    ),
    TRUE ~ scales::dollar(
      stat,
      accuracy = 1
    )
  )
}



# Column shim
column <- function(width, ...){
  colClass <- paste0("col-xl-", width)
  shiny::div(class = colClass, ...)
}



# Highcharts options
set_hcharts_options <- function(...){

  # Dollar suffixes
  hchart_lang <- getOption("highcharter.lang")
  hchart_lang$numericSymbols <- c("k", "m", "b", "t", NULL, NULL)
  options(highcharter.lang = hchart_lang)

}




# load lazy tables for testing
load_tabs <- function(){

  creds <- config::get("dataconnection")

  assign(
    x = "con",
    value = pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = creds$dbname,
      host = creds$host,
      user = creds$user,
      password = creds$password,
      port = creds$port
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
      min = min(date, na.rm = TRUE),
      max = max(date, na.rm = TRUE)
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

  service_trade %>%
    arrange(desc(value)) %>%
    select(service) %>%
    collect() %>%
    pull() %>%
    unique() %>%
    assign("service_categories", ., envir = .GlobalEnv)
}


# Highchart theme
djpr_highcharts <- function(hc){

  highcharter::hc_add_theme(
    hc,
    highcharter::hc_theme(
      chart = list(
        backgroundColor = NULL,
        style = list(
          fontFamily = "VIC-Regular",
          `font-size` = "14px",
          color = "#000000"
          )
        ),
      title = list(
        align = "left",
        # x = 75,
        style = list(
          color = "#000000",
          fontFamily = "VIC-Regular",
          `font-weight` =  "bold",
          `font-size` = "22px",
          `background-color` = "#000000"
        )
      ),
      subtitle = list(
        align = "left",
        # x = 75,
        style = list(
          color = "#000000",
          fontFamily = "VIC-Regular",
          `font-size` = "16px"
        )
      ),
      xAxis = list(
        labels = list(
          style = list(
            color = "#000000",
            `font-weight` =  "bold",
            `font-size` = "12px"
          )
        ),
        title = list(style = list(color = "#000000", `font-weight` =  "bold")),
        lineWidth = 3,
        lineColor = "#000000",
        tickPosition = "inside",
        tickColor = "#000000",
        tickWidth = 3,
        tickLength = 7
      ),
      yAxis = list(
        labels = list(
          style = list(
            color = "#000000",
            `font-weight` =  "bold",
            `font-size` = "12px"
          )
        ),
        title = list(style = list(color = "#000000", `font-weight` =  "bold")),
        minorGridLineWidth = 0,
        lineWidth = 3,
        lineColor = "#000000",
        gridLineColor = "transparent",
        opposite = FALSE
      ),
      navigator = list(
        enabled = FALSE
      ),
      scrollbar = list(
        enabled = FALSE
      ),
      caption = list(
        # x = 75,
        style = list(
          color = "#000000"
        )
      ),
      rangeSelector = list(
        labelStyle = list(color = "#000000"),
        floating = TRUE,
        buttonPosition = list(
          align = "right",
          x = -10,
          y = 10
        )
      ),
      series = list(
        line = list(lineWidth = 4),
        spline = list(lineWidth = 4)
      ),
      exporting = list(enabled = TRUE)
    )
  )

}
