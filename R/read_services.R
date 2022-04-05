
#' Victorian service trade parser
#'
#' @param path location to store temporary download files
#'
#' @return tibble
#' @details Downloads the abs series 'International Trade: Supplementary
#' Information, Calendar Year' tables 3 and 4. The data is filtered for
#' Victorian values
#' @export
#'
#' @examples
#' services <- read_services()
read_services <- function(path = tempdir()){

  # URL construction components
  url_template <- getOption(
    x        = "service_url_template",
    default  = "https://www.abs.gov.au/statistics/economy/international-trade/international-trade-supplementary-information-calendar-year/{year}/{table_code}.xls"
    )

  import_table_code <- getOption("import_table_code", default  = "536805500404")
  export_table_code <- getOption("export_table_code", default  = "536805500403")

  years_to_try <- lubridate::year(Sys.Date())
  years_to_try <- years_to_try:(years_to_try - 3)

  import_url <- glue::glue(
    url_template,
    year = years_to_try,
    table_code = import_table_code
    )

  export_url <- glue::glue(
    url_template,
    year = years_to_try,
    table_code = export_table_code
  )

  # Check workbook existence
  import_url <- import_url[validate_url(import_url)][1]
  export_url <- export_url[validate_url(export_url)][1]


  # Download
  import_path <- paste0(path, "\\imports.xls")
  export_path <- paste0(path, "\\exports.xls")

  utils::download.file(import_url, import_path, mode = "wb")
  utils::download.file(export_url, export_path, mode = "wb")


  # Parse
  imports <- readabs::extract_abs_sheets(
    filename = basename(import_path),
    path = dirname(import_path)
  )

  exports <- readabs::extract_abs_sheets(
    filename = basename(export_path),
    path = dirname(export_path)
  )


  # Extract relevant table
  is_vic_imp <- sapply(imports, function(x) "Victoria" %in% dplyr::pull(x, 1))
  is_vic_exp <- sapply(exports, function(x) "Victoria" %in% dplyr::pull(x, 1))

  imports <- imports[is_vic_imp][[1]]
  exports <- exports[is_vic_exp][[1]]


  # Clean
  head_row_imp <- stringr::str_which(dplyr::pull(imports, 1), "Victoria")
  head_row_exp <- stringr::str_which(dplyr::pull(exports, 1), "Victoria")

  last_row_imp <- stringr::str_which(dplyr::pull(imports, 1), "Total Services")
  last_row_exp <- stringr::str_which(dplyr::pull(exports, 1), "Total Services")

  data_rows_imp <- (head_row_imp + 1):last_row_imp
  data_rows_exp <- (head_row_exp + 1):last_row_exp

  names(imports) <- unlist(imports[head_row_imp, ])
  names(exports) <- unlist(exports[head_row_exp, ])
  names(imports)[1] <- "service"
  names(exports)[1] <- "service"

  imports <- imports[data_rows_imp, ]
  exports <- exports[data_rows_exp, ]


  # Add levels to service categories
  # level_1 <- c(
  #   "Manufacturing services on physical inputs owned by others",
  #   "Maintenance and repair services n.i.e",
  #   "Transport",
  #   "Travel",
  #   "Construction",
  #   "Insurance and Pension services ",
  #   "Financial Services ",
  #   "Charges for the use of intellectual property n.i.e",
  #   "Telecommunications, computer and information services",
  #   "Other business services",
  #   "Personal, cultural, and recreational services",
  #   "Government goods and services n.i.e "
  # )
  #
  # level_2 <-

}
