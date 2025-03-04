#' Download, tidy and import ABS balance of payments data
#'
#' Downloads ABS Balance of Payments (5302.0) Tables 21 and 22, goods and
#' services credits and debits by state.
#' @param path Ignored.
#' @return A tibble containing both imports (debits) and credits (exports)
#' data
#' @examples
#' \dontrun{
#' read_bop()
#' }
#' @export

read_bop <- function(path = stop("ignored")) {
  credits <- with_fileset("credits", load_abs)
  credits[, exports_imports := "Exports"]

  debits <- with_fileset("debits", load_abs)
  debits[, exports_imports := "Imports"]

  bop <- data.table::rbindlist(list(credits, debits))
  bop <- bop[`Series Type` == "Seasonally Adjusted" & !is.na(value)]
  bop <- bop |>
    tidyr::separate(
      `Data Item Description`
      ,c("indicator", "goods_services", "state")
      ," *[;,] *"
      ,extra="drop"
    ) |>
    data.table::setDT()
  bop[, list(
    exports_imports, indicator, goods_services, state, date, value
    ,series_id=`Series ID`,unit=Unit
  )]
}

# Loads ABS spreadsheet into a data.table that looks like:
# -------------------------------------------------
# | date | value | ...other fields in Index sheet |
# -------------------------------------------------
load_abs <- function(path) {
  index <- readxl::read_excel(
    path, "Index", skip=9, .name_repair="unique_quiet"
  )
  # Drop NA cols/rows
  index <- index |>
    dplyr::filter(!is.na(`Series ID`)) |>
    dplyr::select(-starts_with("..."))

  sheets <- readxl::excel_sheets(path)
  sheets <- stringr::str_subset(sheets, "^Data")
  data_sheets <- lapply(sheets, \(sh) {
    readxl::read_excel(path, sh, skip=9) |>
      dplyr::rename(date = `Series ID`) |>
      tidyr::pivot_longer(-date, names_to="Series ID", values_to="value")
  }) |>
    data.table::rbindlist()

  merge(data_sheets, index, by="Series ID")
}
