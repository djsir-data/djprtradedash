TABLES_DIR <- "inst/tables"

data_save <- function(dataset, name) {
  dir.create(TABLES_DIR, showWarnings = F, recursive = T)
  arrow::write_feather(dataset, sprintf("inst/tables/%s.feather", name))
}

data_load <- function(env = .GlobalEnv) {
  if(!is.null(env$data_loaded)) {
    # Already loaded, skip
    return()
  }

  list.files(TABLES_DIR) |> Map(dir=TABLES_DIR, f=\(dir, fn) {
    data <- arrow::read_feather(file.path(dir, fn))
    # factors -> strings
    data <- mutate_if(data, is.factor, as.character)
    name <- tools::file_path_sans_ext(fn)
    env[[name]] <- data
  })

  # Returns data.frame(min=min(x), max=max(x))
  range_df <- function(x) {
    as.data.frame(
      as.list(range(x, na.rm=T))
      ,col.names=c("min", "max")
    )
  }
  env$merch_dates <- range_df(env$merch$date)
  env$merch_sitc_lu <-
    dplyr::distinct(env$merch, sitc, sitc_code) |>
    mutate(n=nchar(sitc_code))
  env$merch_country_dest <- unique(env$merch$country_dest)

  env$bop_dates <- range_df(env$bop$date)
  env$service_dates <- range_df(env$service_trade$date)
  env$service_categories <-
    arrange(env$service_trade, desc(value)) |>
    distinct(service) |>
    pull(service)

  env$data_loaded <- T
}
