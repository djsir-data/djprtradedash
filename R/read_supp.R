#' Download, tidy and import ABS International Trade Supplementary Information
#'
#' Downloads Tables 1 through 8, inculsive, of the ABS International Trade: Supplementary Information data.
#' @param format Selects whether calendar year or financial year data is imported
#' @param table_no Selects which tables from the ABS will be imported
#' @param list Where multiple tables are requested, if list is TRUE, table outputs exported separately in a list, if FALSE tables are exported in a combined tibble.
#' @param path Path where Excel files downloaded from the ABS should be stored
#' @return A tibble containing the relevant table, or a list of tibbles if more than one ABS table has been requested
#' @examples
#' read_supp()
#' @export

read_supp <- function(format = c("cy", "fy"),
                      table_no = c(1, 2, 3, 4, 5, 6, 7, 8), list = FALSE, path = tempdir()) {
  format <- match.arg(format)

  # Create a temp directory specific to this format + table no combination
  temp_dir <- file.path(
    tempdir(),
    paste(format,
      paste0(table_no, collapse = ""),
      sep = "_"
    )
  )

  if (!dir.exists(temp_dir)) dir.create(temp_dir)

  on.exit(unlink(temp_dir))

  if (format == "cy") {
    suppressMessages(
      readabs::download_abs_data_cube(
        "international-trade-supplementary-information-calendar-year",
        "zip",
        temp_dir)
    )
  }

  if (format == "fy") {
    suppressMessages(
      readabs::download_abs_data_cube(
        "international-trade-supplementary-information-financial-year",
        "zip",
        temp_dir)
    )
  }

  table_no <- unique(table_no)

  file_name <- list.files(temp_dir)[grepl("zip", list.files(temp_dir))]
  utils::unzip(file.path(temp_dir, file_name), exdir = temp_dir)
  excel_files <- list.files(path = temp_dir, pattern = "*.xls", full.names = TRUE)

  copy_result <- file.copy(
    from = excel_files,
    to = path,
    overwrite = TRUE
  )

  stopifnot(all(copy_result))

  file_no <- length(excel_files)

  tables <- vector(mode = "list", length = file_no)

  year_output <- vector(mode = "list", length = file_no)

  extract_files <- lapply(
    basename(excel_files),
    readabs::extract_abs_sheets,
    path = temp_dir
  )


  for (j in 2:file_no - 1) {
    year_files <- extract_files[[j]]

    tables[[j]] <- matrix(1, length(year_files) - 1)

    for (i in 2:length(year_files) - 1) {
      temp_table <- year_files[[i + 1]]
      names(temp_table) <- as.character(tidyr::drop_na(temp_table)[1, ])
      series <- temp_table[3, 1]
      header_row <- -which(temp_table[, 1] == as.character(utils::head(tidyr::drop_na(temp_table), n = 1)[, 1]))
      footer_row <- which(temp_table[, 1] == as.character(utils::tail(tidyr::drop_na(temp_table), n = 1)[, 1])) - nrow(temp_table)
      temp_table <- utils::tail(utils::head(temp_table, footer_row), header_row) %>%
        tidyr::gather("year", "value", 2:ncol(temp_table))
      temp_table[, "subset"] <- rep(stringr::word(gsub("\\s*\\([^\\)]+\\)", "", series), -1), nrow(temp_table))
      temp_table[, "abs_series"] <- rep(as.character(series), nrow(temp_table))
      temp_table <- dplyr::mutate(temp_table, value = suppressWarnings(as.numeric(.data$value)))
      names(temp_table)[1] <- "item"
      assign(paste0("table_", j, ".", i), temp_table)
      tables[[j]][i] <- paste0("table_", j, ".", i)
      rm(header_row)
      rm(footer_row)
      rm(temp_table)
      rm(i)
    }

    year_output[[j]] <- do.call("rbind", lapply(tables[[j]], as.name))
  }
  if (list == TRUE) {
    if (length(year_output[table_no]) == 1) {
      year_output[table_no][[1]]
    } else {
      year_output[table_no]
    }
  }
  if (list == FALSE) {
    dplyr::bind_rows(year_output[table_no])
  }
}
