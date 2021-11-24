#' Create ABS Lookup table for merchandise exports data
#'
#' The ABS merchandise exports data
#' (\url{https://stat.data.abs.gov.au/index.aspx?DatasetCode=MERCH_EXP}) is
#' supplied with short codes for various data fields, such as industry and
#' SITC code. This function creates a series of lookup tables that include
#' English descriptions of these fields.
#' @param path Directory in which downloaded XML should be stored
#' @return A list, each element of which is a `tbl_df`
#' @export
#' @examples
#' create_merch_lookup()
create_merch_lookup <- function(path = tempdir()) {
  url <- "https://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/MERCH_EXP"

  file <- file.path(tempdir(), "lookup.xml")

  utils::download.file(
    url = url,
    destfile = file,
    mode = "wb",
    quiet = TRUE
  )

  lookup <- readsdmx::read_sdmx(file) %>%
    dplyr::as_tibble()

  lookup <- lookup %>%
    dplyr::filter(.data$en %in% c(
      "State of Origin",
      "Commodity by SITC",
      "Industry of Origin (ANZSIC06)",
      "Country of Destination"
    )) %>%
    dplyr::select(.data$en,
      desc = .data$en_description,
      parent_code = .data$parentCode,
      .data$value
    )

  lookup <- lookup %>%
    dplyr::mutate(
      desc =
        dplyr::if_else(
          .data$en == "State of Origin" &
            .data$desc == "Total",
          "Australia",
          .data$desc
        )
    )

  lookup <- lookup %>%
    dplyr::mutate(en = dplyr::case_when(
      .data$en == "Country of Destination" ~ "country",
      .data$en == "State of Origin" ~ "region",
      .data$en == "Industry of Origin (ANZSIC06)" ~ "industry",
      .data$en == "Commodity by SITC" ~ "sitc_rev3"
    ))

  lookup <- lookup %>%
    dplyr::group_by(.data$en)

  lookup_groups <- lookup %>%
    dplyr::group_keys() %>%
    dplyr::pull()

  lookup <- lookup %>%
    dplyr::group_split()

  lookup <- stats::setNames(lookup, lookup_groups)

  lookup <- purrr::imap(
    lookup,
    ~ tidyr::pivot_wider(
      data = .x,
      names_from = "en",
      values_from = "value"
    ) %>%
      dplyr::rename("{.y}_desc" := .data$desc) %>%
      dplyr::select(-.data$parent_code) %>%
      dplyr::distinct()
  )

  lookup
}
