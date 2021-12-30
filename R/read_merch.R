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
#' @examples
#' \dontrun{
#' read_merch()
#' }
#' @export
#' @import data.table
#' @return A tibble containing merchandise export data


read_merch <- function(path = tempdir(),
                       max_date = Sys.Date(),
                       min_date = as.Date("2010-01-01"),
                       series = "export") {
  
  if (series == "export") {
    url <- "https://www.abs.gov.au/websitedbs/D3110132.nsf/home/DataExplorer/$File/MERCH_EXP.zip"
    exports_dest_zip <- file.path(path, basename(url))
    download.file(url, exports_dest_zip, mode="wb")
    unzip(exports_dest_zip, exdir=path, unzip="unzip")
    exports_data <- data.table::fread(file.path(path, "MERCH_EXP.csv"))

    merch <- exports_data[,.(
      sitc = `COMMODITY_SITC: Commodity by SITC`,
      country_dest = `COUNTRY_DEST: Country of Destination`,
      origin = `STATE_ORIGIN: State of Origin`,
      date = `TIME_PERIOD: Time Period`,
      value = `OBS_VALUE`,
      unit = `UNIT_MULT: Unit of Multiplier`
      )]

    merch[, ':='(
      sitc_code = stringr::str_split_fixed(sitc, ": ", 2)[,1],
      sitc = stringr::str_split_fixed(sitc, ": ", 2)[,2],
      country_code = stringr::str_split_fixed(country_dest, ": ", 2)[,1],
      country_dest = stringr::str_split_fixed(country_dest, ": ", 2)[,2],
      origin = stringr::str_split_fixed(origin, ": ", 2)[,2],
      unit = stringr::str_split_fixed(unit, ": ", 2)[,2],
      date = as.Date(paste0(date, "-01")),
      export_import = series
      )]

    merch[origin == "Total"] <- merch[, origin := "Australia"]

    merch <- merch[order(origin, 
                   sitc, 
                   country_dest, 
                   date)]

    merch <- merch[date >= min_date & date <= max_date]
    
    merch <- unique(merch)
  }

  if (series == "import") {
    url <- "https://www.abs.gov.au/websitedbs/D3110132.nsf/home/DataExplorer/$File/MERCH_IMP.zip"
    imports_dest_zip <- file.path(path, basename(url))
    download.file(url, imports_dest_zip, mode="wb")
    unzip(imports_dest_zip, exdir=path, unzip="unzip")
    imports_data <- data.table::fread(file.path(path, "MERCH_IMP.csv"))

    merch <- imports_data[,.(
      sitc = `COMMODITY_SITC: Commodity by SITC`,
      country_origin = `COUNTRY_ORIGIN: Country of Origin`,
      dest = `STATE_DEST: State of Destination`,
      date = `TIME_PERIOD: Time Period`,
      value = `OBS_VALUE`,
      unit = `UNIT_MULT: Unit of Multiplier`
      )]

    merch[, ':='(
      sitc_code = stringr::str_split_fixed(sitc, ": ", 2)[,1],
      sitc = stringr::str_split_fixed(sitc, ": ", 2)[,2],
      country_code = stringr::str_split_fixed(country_origin, ": ", 2)[,1],
      country_origin = stringr::str_split_fixed(country_origin, ": ", 2)[,2],
      dest = stringr::str_split_fixed(dest, ": ", 2)[,2],
      unit = stringr::str_split_fixed(unit, ": ", 2)[,2],
      date = as.Date(paste0(date, "-01")),
      export_import = series
      )]

    merch[dest == "Total"] <- merch[, dest := "Australia"]

    merch <- merch[order(dest, 
                   sitc, 
                   country_origin, 
                   date)]

    merch <- merch[date >= min_date & date <= max_date]
    
    merch <- unique(merch)
  }
  merch
}