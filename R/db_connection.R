# Assign multiple lazy data frames to global environement
assign_table_global <- function(con, tables){
  lapply(tables, function(tab){
    assign(tab, dplyr::tbl(con, tab), envir = .GlobalEnv)
  })
}



# load lazy tables for testing
load_tabs <- function(){

  # Get environment-spesific connection information
  creds <- config::get("dataconnection")

  # If running in cached database mode, load data
  if(creds$use_DBcache == TRUE & !exists("merch", envir = .GlobalEnv)){
    load("DBcache.rData", envir = .GlobalEnv)
    return(NULL)
  }

  # Cache mode and data loaded
  if(creds$use_DBcache == TRUE & exists("merch", envir = .GlobalEnv)){
    if(inherits(merch, "tbl_lazy")){
      load("DBcache.rData", envir = .GlobalEnv)
      return(NULL)
    } else {
      return(NULL)
    }
  }

  # Do not connect if there is already a working connection
  if(exists("con", envir = .GlobalEnv)){
    if(pool::dbIsValid(con)){
      return(NULL)
    }
  }

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
    dplyr::summarise(
      min = min(date, na.rm = TRUE),
      max = max(date, na.rm = TRUE)
    ) %>%
    dplyr::collect()  %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.Date)) %>%
    assign("service_dates", ., envir = .GlobalEnv)

  service_trade %>%
    arrange(desc(value)) %>%
    collect() %>%
    dplyr::select(service) %>%
    pull() %>%
    unique() %>%
    assign("service_categories", ., envir = .GlobalEnv)
}
