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
  url <- "https://api.data.abs.gov.au/dataflow/ABS/MERCH_EXP/1.0.0?references=all&detail=referencepartial"

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
      "Merchandise State",
      "Merchandise Commodity by SITC revision 3",
      "Merchandise Country"
    )) %>%
    dplyr::select(.data$en,
      desc = .data$en_description,
      code = .data$id_description
    )

  lookup <- lookup %>%
    dplyr::mutate(
      desc =
        dplyr::if_else(
          .data$en == "Merchandise State" &
            .data$desc == "Total",
          "Australia",
          .data$desc
        )
    )

  lookup <- lookup %>%
    dplyr::mutate(en = dplyr::case_when(
      .data$en == "Merchandise Country" ~ "country",
      .data$en == "Merchandise State" ~ "region",
      .data$en == "Merchandise Commodity by SITC revision 3" ~ "sitc_rev3"
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
      values_from = "desc"
    ) %>%
      dplyr::rename("{.y}_code" := .data$code) %>%
      dplyr::distinct()
  )

  lookup
}
