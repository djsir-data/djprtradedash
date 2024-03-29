#' Download merchandise exports data
#'
#' Obtains merchandise exports data from ABS Data Explorer
#' (\url{https://explore.data.abs.gov.au/?tm=Merchandise&pg=0}).
#'
#' @details The ABS will not supply data frames of over 1m rows using the
#' ABS.Stat API. For this reason, you cannot download more than 1 year at a
#' time worth of data using this function, as this is around the point at which
#' the 1m row limit is reached.
#' @param path Path to directory where CSV files are extracted and stored
#' @param min_date The minimum date to include in your data
#' @param max_date The maximum date to include in your data
#' @param series Selects whether import or export merchandise data is downloaded
#'
#' @importFrom httr GET write_disk http_status
#' @importFrom assertthat assert_that
#' @examples
#' \dontrun{
#' read_merch()
#' }
#' @export
#' @return A tibble containing merchandise export data


read_merch <- function(path = tempdir(),
                       max_date = Sys.Date(),
                       min_date = as.Date("2000-01-01"),
                       series = c("export","import")) {

  series <- match.arg(series)

  url <- switch(
    series,
    export = "https://api.data.abs.gov.au/files/ABS_MERCH_EXP_1.0.0.csv",
    import = "https://api.data.abs.gov.au/files/ABS_MERCH_IMP_1.0.0.csv"
    )

  dest <- file.path(path, basename(url), fsep = .Platform$file.sep)

  #download.file(url, dest, mode = "wb")

  resp <- httr::GET(url, httr::write_disk(dest, overwrite=TRUE))
  status <- httr::http_status(resp)

  assertthat::assert_that(status$category == "Success",
                          msg = glue('Download Failed with message: {status$message}'))

  merch <- data.table::fread(
    dest,
    stringsAsFactors = TRUE,
    data.table = TRUE
  )

  if (series == "export") {

    data.table::setnames(
      merch,
      c(
        "COMMODITY_SITC: Commodity by SITC",
        "COUNTRY_DEST: Country of Destination",
        "STATE_ORIGIN: State of Origin",
        "TIME_PERIOD: Time Period",
        "OBS_VALUE",
        "UNIT_MULT: Unit of Multiplier"
        ),
      c(
        "sitc",
        "country_dest",
        "origin",
        "date",
        "value",
        "unit"
      )
      )

    merch[, c("sitc_code", "sitc") := tstrsplit_factor(sitc, ": ")]
    merch[, c("country_code", "country_dest") := tstrsplit_factor(country_dest, ": ")]
    merch[, `:=`(
      origin = tstrsplit_factor(origin, ": ")[[2]],
      unit = tstrsplit_factor(unit, ": ")[[2]],
      date = lubridate::ymd(paste0(date, "-01")),
      export_import = series
    )]
    merch[origin == "Total", origin := "Australia"]
    merch <- merch[order(origin,
                   sitc,
                   country_dest,
                   date)]
    merch <- merch[date >= min_date & date <= max_date]
    merch <- unique(merch)
    merch <- merch[, .(sitc, country_dest, origin, date, value, unit, sitc_code, country_code, export_import)]

    }

  if (series == "import") {

    data.table::setnames(
      merch,
      c(
        "COMMODITY_SITC: Commodity by SITC",
        "COUNTRY_ORIGIN: Country of Origin",
        "STATE_DEST: State of Destination",
        "TIME_PERIOD: Time Period",
        "OBS_VALUE",
        "UNIT_MULT: Unit of Multiplier"
      ),
      c(
        "sitc",
        "country_origin",
        "dest",
        "date",
        "value",
        "unit"
      )
    )

    merch[, c("sitc_code", "sitc") := tstrsplit_factor(sitc, ": ")]
    merch[, c("country_code", "country_origin") := tstrsplit_factor(country_origin, ": ")]
    merch[, `:=`(
      dest = tstrsplit_factor(dest, ": ")[[2]],
      unit = tstrsplit_factor(unit, ": ")[[2]],
      date = lubridate::ymd(paste0(date, "-01")),
      export_import = series
    )]
    merch[dest == "Total", dest := "Australia"]
    merch <- merch[order(dest,
                   sitc,
                   country_origin,
                   date)]
    merch <- merch[date >= min_date & date <= max_date]
    merch <- unique(merch)
    merch <- merch[, .(sitc, country_origin, dest, date, value, unit, sitc_code, country_code, export_import)]

    }

  return(merch)

}
