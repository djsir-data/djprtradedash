#' Download merchandise exports data
#'
#' Obtains merchandise exports data from ABS Data Explorer
#' (\url{https://explore.data.abs.gov.au/?tm=Merchandise&pg=0}).
#'
#' @details The ABS will not supply data frames of over 1m rows using the
#' ABS.Stat API. For this reason, you cannot download more than 1 year at a
#' time worth of data using this function, as this is around the point at which
#' the 1m row limit is reached.
#' @param path Ignored
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


read_merch <- function(path = stop("ignored"),
                       max_date = Sys.Date(),
                       min_date = as.Date("2000-01-01"),
                       series = c("export","import")) {

  series <- match.arg(series)
  merch <- with_fileset(series, data.table::fread, stringsAsFactors=TRUE)

  # Helper - pick one or the other for exports/imports
  series_switch <- function(export, import) {
    switch(series, export=export, import=import)
  }

  # This relies on lazy evaluation to avoid undefined column errors
  merch <- merch[, .(
    sitc = `Commodity by SITC`,
    # placeholder name
    country = series_switch(`Country of Destination`, `Country of Origin`),
    # placeholder name
    state = series_switch(`State of Origin`, `State of Destination`),
    date = lubridate::ym(TIME_PERIOD),
    value = OBS_VALUE,
    unit = `Unit of Multiplier`,
    sitc_code = COMMODITY_SITC,
    country_code = series_switch(COUNTRY_DEST, COUNTRY_ORIGIN),
    export_import = series
  )]
  merch[state == "Total", state := "Australia"]
  data.table::setorder(merch, state, sitc, country, date)
  merch <- merch[min_date <= date & date <= max_date]
  merch <- unique(merch)
  renamed <- series_switch(
    c("country_dest", "origin"),
    c("country_origin", "dest")
  )
  data.table::setnames(merch, c("country", "state"), renamed)

  merch
}
