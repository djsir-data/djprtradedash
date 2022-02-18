viz_merch_explorer <- function(data = merch,
                               countries = c("Thailand", "Malaysia"),
                               goods = "Medicinal and pharmaceutical products",
                               origin = "Victoria",
                               facet_by = "country_dest",
                               smooth = FALSE) {
  dates <- unique(data$date) 

  df <- data %>%
    dplyr::filter(
      .data$sitc %in% .env$goods,
      .data$country_dest %in% .env$countries,
      .data$origin == .env$origin
    ) %>%
    dplyr::mutate(
      group = paste(.data$country_dest, .data$sitc, sep = "-"),
      sitc = as.character(.data$sitc),
      country_dest = as.character(.data$country_dest)
    )

  combs <- df %>% dplyr::select(-date, -value) %>% unique()
  
  df <- bind_rows(
    merge(dates, combs) %>% 
    rename(date = 1) %>%
    mutate(value = 0),
    df
    ) %>%
  group_by(group, date) %>%
  slice(n()) %>%
  ungroup()

  if (facet_by == "country_dest") {
    df <- df %>%
      dplyr::mutate(col = .data$sitc)
  } else {
    df <- df %>%
      dplyr::mutate(col = .data$country_dest)
  }

  if (smooth) {
    df <- df %>%
      dplyr::group_by(.data$group) %>%
      dplyr::arrange(.data$date) %>%
      dplyr::mutate(value = slide_mean(.data$value, before = 11L))
  }

  n_col <- length(unique(df$col))
  cols <- if (n_col <= 10) {
    suppressWarnings(djprtheme::djpr_pal(n_col))
  } else {
    suppressWarnings(grDevices::colorRampPalette(djprtheme::djpr_pal(10))(n_col))
  }

  total_col_chars <- df$col %>%
    unique() %>%
    paste(collapse = " ") %>%
    nchar()

  show_legend <- if_else(total_col_chars >= 100,
    TRUE, FALSE
  )

  date_limits <- c(min(data$date), max(data$date))
  x_breaks <- djprtheme::breaks_right(
    limits = date_limits,
    n_breaks = 5
  )

  p <- df %>%
    ggplot(aes(
      x = .data$date,
      y = .data$value,
      col = .data$col,
      group = .data$group
    )) +
    geom_line() +
    geom_point(
      data = ~ dplyr::group_by(., .data$group) %>%
        dplyr::filter(.data$date == max(.data$date)),
      fill = "white",
      show.legend = FALSE,
      stroke = 1.5, size = 2.5, shape = 21
    ) +
    scale_colour_manual(values = cols) +
    facet_wrap(facets = facet_by)

  if (show_legend) {
    p <- p +
      theme_djpr(legend = "top") +
      theme(
        legend.direction = "vertical",
        legend.text = element_text(size = 11)
      ) +
      scale_x_date(
        breaks = x_breaks,
        date_labels = "%b\n%Y"
      )
  } else {
    days_in_data <- as.numeric(max(data$date) - min(data$date))
    p <- p +
      ggrepel::geom_text_repel(
        data = ~ filter(
          .,
          .data$date == max(.data$date)
        ),
        aes(label = stringr::str_wrap(
          .data$col,
          10
        )),
        size = 11 / .pt,
        lineheight = 0.9,
        hjust = 0,
        nudge_x = days_in_data * 0.033,
        seed = 123, show.legend = FALSE,
        direction = "y"
      ) +
      scale_x_date(
        expand = expansion(mult = c(0, 0.28)),
        breaks = x_breaks, date_labels = "%b\n%Y"
      ) +
      theme_djpr()
  }

  p
}
