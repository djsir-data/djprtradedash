viz_merch_explorer <- function(data = merch,
                               countries = c("Thailand", "Malaysia"),
                               goods = "Medicinal and pharmaceutical products",
                               origin = "Victoria",
                               facet_by = "country_dest",
                               smooth = FALSE) {
  df <- data %>%
    dplyr::filter(
      .data$sitc_rev3 %in% .env$goods,
      .data$country_dest %in% .env$countries,
      .data$origin == .env$origin
    ) %>%
    dplyr::mutate(
      group = paste(.data$country_dest, .data$sitc_rev3, sep = "-"),
      sitc_rev3 = as.character(.data$sitc_rev3),
      country_dest = as.character(.data$country_dest)
    )

  if (facet_by == "country_dest") {
    df <- df %>%
      dplyr::mutate(col = .data$sitc_rev3)
  } else {
    df <- df %>%
      dplyr::mutate(col = .data$country_dest)
  }

  if (smooth) {
    df <- df %>%
      dplyr::group_by(.data$group) %>%
      dplyr::arrange(.data$date) %>%
      dplyr::mutate(value = slider::slide_mean(.data$value, before = 11L))
  }

  # df %>%
  #   djpr_ts_linechart(
  #     col_var = .data$col,
  #     group_var = .data$group,
  #     label_single_line = TRUE
  #   ) +
  #   facet_wrap(facets = facet_by)

  n_col <- length(unique(df$col))
  cols <- if (n_col <= 10) {
      suppressWarnings(djprtheme::djpr_pal(n_col))
    } else {
      suppressWarnings(grDevices::colorRampPalette(djprtheme::djpr_pal(10))(n_col))

    }

  df %>%
    ggplot(aes(x = .data$date,
               y = .data$value,
               col = .data$col,
               group = .data$group)) +
    geom_line() +
    scale_colour_manual(values = cols) +
    facet_wrap(facets = facet_by) +
    theme_djpr()

}
