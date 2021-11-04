viz_merch_explorer <- function(data = merch,
                              countries = "Thailand",
                              goods = "Medicinal and pharmaceutical products",
                              origin = "Victoria",
                              facet_by = "country_dest"
                              ) {

  df <- data %>%
    dplyr::filter(.data$sitc_rev3 %in% .env$goods,
                  .data$country_dest %in% .env$countries,
                  .data$origin == .env$origin) %>%
    dplyr::mutate(group = paste(.data$country_dest, .data$sitc_rev3, sep = "-"),
                  col = ifelse(facet_by == "country_dest",
                               as.character(.data$sitc_rev3),
                               as.character(.data$country_dest)))

  df %>%
    djpr_ts_linechart(col_var = .data$col,
                      group_var = .data$group,
                      label_single_line = TRUE) +
    facet_wrap(facets = facet_by)
}
