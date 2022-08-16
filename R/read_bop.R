#' Download, tidy and import ABS balance of payments data
#'
#' Downloads ABS Balance of Payments (5302.0) Tables 21 and 22, goods and
#' services credits and debits by state.
#' @param path Path where Excel files from the ABS should be stored
#' @return A tibble containing both imports (debits) and credits (exports)
#' data
#' @examples
#' \dontrun{
#' read_bop()
#' }
#' @export

read_bop <- function(path = tempdir()) {
  suppressMessages(
    readabs::download_abs_data_cube(
      "balance-payments-and-international-investment-position-australia",
      "21.xls",
      path
    )
  )

  credits <- suppressMessages(
    readabs::read_abs_local(
      path = path,
      filenames = list.files(path)[grepl("21.xls", list.files(path))]
    )
  ) %>%
    dplyr::mutate(series = paste("Exports", series, sep = " ; "))
  unlink(file.path(path, list.files(path)[grepl("21.xls", list.files(path))]))

  suppressMessages(
    readabs::download_abs_data_cube(
      "balance-payments-and-international-investment-position-australia",
      "22.xls",
      path
    )
  )

  debits <- suppressMessages(
    readabs::read_abs_local(
      path = path,
      filenames = list.files(path)[grepl("22.xls", list.files(path))]
    )
  ) %>%
    dplyr::mutate(series = paste("Imports", series, sep = " ; "))

  bop <- dplyr::bind_rows(credits, debits)

  bop <- bop %>%
    dplyr::filter(series_type == "Seasonally Adjusted")

  bop <- bop %>%
    dplyr::select(
      series,
      date,
      value,
      series_id,
      unit
    )

  bop %>%
    tidyr::separate(series,
      into = c(
        "exports_imports",
        "indicator",
        "goods_state"
      ),
      extra = "drop",
      sep = ";"
    ) %>%
    tidyr::separate(goods_state,
      into = c(
        "goods_services",
        "state"
      ),
      sep = ","
    ) %>%
    dplyr::mutate(dplyr::across(
      !dplyr::one_of(c(
        "date",
        "value",
        "series_id",
        "unit"
      )),
      ~ trimws(.x, "both")
    )) %>%
    dplyr::filter(!is.na(value))
}
