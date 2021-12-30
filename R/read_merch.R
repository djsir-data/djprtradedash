#' Download merchandise exports data
#'
#' Obtains merchandise exports data from ABS.Stat
#' (\url{https://stat.data.abs.gov.au/index.aspx?DatasetCode=MERCH_EXP}).
#'
#' @details The ABS will not supply data frames of over 1m rows using the
#' ABS.Stat API. For this reason, you cannot download more than 1 year at a
#' time worth of data using this function, as this is around the point at which
#' the 1m row limit is reached.
#' @param path Path to directory where XML files should be stored
#' @param min_date The minimum date to include in your data
#' @param max_date The maximum date to include in your data
#' @param series Selects whether import or export merchandise data is downloaded
#' @param check_local Logical. Check if a local version of the requested data is
#' available at the `path` location; if present it will be loaded.
#' @param merch_lookup A list of tibbles containing short and long versions
#' of various data entries; see `create_merch_lookup()`.
#' @examples
#' \dontrun{
#' read_merch()
#' }
#' @export
#' @return A tibble containing merchandise export data


read_merch <- function(path = tempdir(),
                       max_date = Sys.Date(),
                       min_date = max_date - 180,
                       series = "export",
                       check_local = TRUE,
                       merch_lookup = create_merch_lookup()) {

  
  
  if (max_date - min_date > 180) {
    message("WARNINGS: Downloading more than 6 months worth of data at a time may fail due to ABS server limits.")
  }
  if (Sys.Date() - min_date < 90){
    message("WARNING: The ABS server may not have data less than 3 months from the current date.")
  }

  min_month <- format(min_date, "%Y-%m")
  max_month <- format(max_date, "%Y-%m")
  
  if (series == "export") {

    codes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, "F")
    code_names <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "NA", "REEXP")

    agg_merch <- vector(mode = "list", length = length(codes))

    for (i in 1:length(codes)){
      tryCatch({
      url <- paste0(
        "https://api.data.abs.gov.au/data/ABS,MERCH_EXP,1.0.0/..",
        codes[i],
        ".M?startPeriod=",
        min_month,
        "&endPeriod=",
        max_month
        )

      file <- file.path(
        path,
        paste0("abs_merch_exp_",
          as.character(min_date),
          "_",
          as.character(max_date),
          "_",
          code_names[i],
          ".xml")
        )

      if (isFALSE(check_local) || !file.exists(file)) {
        message(
          "Downloading ", code_names[i], " ", series, " merchandise trade data from ", min_month, " to ",
          max_month
        )
        utils::download.file(
          url,
          file
        )
      } else {
        message("Loading merchandise trade from local file:\n", file)
        }

      safely_read_sdmx <- purrr::safely(readsdmx::read_sdmx)

      merch <- safely_read_sdmx(file)

      # Check to see if merch downloaded and imported without error
      if (is.null(merch$error)) {
        merch <- merch$result
      } else {
        # If file did not load, try again one more time
        utils::download.file(
          url,
          file
        )
        merch <- safely_read_sdmx(path)

        # If it failed the second time, give an informative error
        if (!is.null(merch$error)) {
          stop(paste("Could not download and import", url))
        }

        merch <- merch$result
      }

      merch <- merch %>%
        dplyr::as_tibble()

      names(merch) <- tolower(names(merch))

      agg_merch[[i]] <- merch 
      }, error = function(e){})
    }

    merch <- dplyr::bind_rows(agg_merch)

    # merch <- merch %>%
    #   dplyr::filter(.data$industry == "-1")

    merch <- merch %>%
      transmute(sitc_rev3_code = commodity_sitc,
                country_code = country_dest,
                time = obsdimension,
                obsvalue = obsvalue,
                region_code = state_origin)

    merch <- merch %>%
      dplyr::select(.data$country_code,
        # .data$industry,
        .data$sitc_rev3_code,
        .data$time,
        .data$region_code,
        value = .data$obsvalue
      )

    merch <- merch %>%
      dplyr::mutate(
        value = as.numeric(.data$value),
        unit = "000s"
      )

    merch_lookup <- merch_lookup %>% purrr::list_modify("industry" = NULL)
    merch <- suppressMessages(
      purrr::reduce(
        .x = c(list(merch), merch_lookup),
        .f = dplyr::left_join
      )
    )

    merch <- merch %>%
      dplyr::mutate(date = lubridate::ymd(paste0(.data$time, "-01"))) %>%
      dplyr::select(.data$date,
        country_dest = .data$country,
        sitc_rev3 = .data$sitc_rev3,
        sitc_rev3_code = .data$sitc_rev3_code,
        origin = .data$region,
        .data$unit,
        .data$value
      )

    merch <- merch %>%
      dplyr::arrange(
        .data$origin,
        .data$sitc_rev3,
        .data$country_dest,
        .data$date
      )

    merch <- merch %>% 
      filter(!is.na(country_dest)) %>%
      mutate(export_import = series)
  }

  if (series == "import") {

    codes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    code_names <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "NA")

    agg_merch <- vector(mode = "list", length = length(codes))

    for (i in 1:length(codes)){
      url <- paste0(
        "https://api.data.abs.gov.au/data/ABS,MERCH_IMP,1.0.0/..",
        codes[i],
        ".M?startPeriod=",
        min_month,
        "&endPeriod=",
        max_month
        )

      file <- file.path(
        path,
        paste0("abs_merch_imp_",
          as.character(min_date),
          "_",
          as.character(max_date),
          "_",
          code_names[i],
          ".xml")
        )

      if (isFALSE(check_local) || !file.exists(file)) {
        message(
          "Downloading ", code_names[i], " ", series, " merchandise trade data from ", min_month, " to ",
          max_month
        )
        utils::download.file(
          url,
          file
        )
      } else {
        message("Loading merchandise trade from local file:\n", file)
        }

      safely_read_sdmx <- purrr::safely(readsdmx::read_sdmx)

      merch <- safely_read_sdmx(file)

      # Check to see if merch downloaded and imported without error
      if (is.null(merch$error)) {
        merch <- merch$result
      } else {
        # If file did not load, try again one more time
        utils::download.file(
          url,
          file
        )
        merch <- safely_read_sdmx(path)

        # If it failed the second time, give an informative error
        if (!is.null(merch$error)) {
          stop(paste("Could not download and import", url))
        }

        merch <- merch$result
      }

      merch <- merch %>%
        dplyr::as_tibble()

      names(merch) <- tolower(names(merch))

      agg_merch[[i]] <- merch
    }

    merch <- dplyr::bind_rows(agg_merch)





    merch <- merch %>%
      transmute(sitc_rev3_code = commodity_sitc,
                country_code = country_origin,
                time = obsdimension,
                obsvalue = obsvalue,
                region_code = state_dest)

    merch <- merch %>%
      dplyr::select(.data$country_code,
        # .data$industry,
        .data$sitc_rev3_code,
        .data$time,
        .data$region_code,
        value = .data$obsvalue
      )

    merch <- merch %>%
      dplyr::mutate(
        value = as.numeric(.data$value),
        unit = "000s"
      )

    merch_lookup <- merch_lookup %>% purrr::list_modify("industry" = NULL)
    merch <- suppressMessages(
      purrr::reduce(
        .x = c(list(merch), merch_lookup),
        .f = dplyr::left_join
      )
    )

    merch <- merch %>%
      dplyr::mutate(date = lubridate::ymd(paste0(.data$time, "-01"))) %>%
      dplyr::select(.data$date,
        country_origin = .data$country,
        sitc_rev3 = .data$sitc_rev3,
        sitc_rev3_code = .data$sitc_rev3_code,
        dest = .data$region,
        .data$unit,
        .data$value
      )

    merch <- merch %>%
      dplyr::arrange(
        .data$dest,
        .data$sitc_rev3,
        .data$country_origin,
        .data$date
      )

    merch <- merch %>% 
      filter(!is.na(country_origin)) %>%
      mutate(export_import = series)
  }
  merch
}