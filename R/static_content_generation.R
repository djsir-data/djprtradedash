

djpr_table <- function(df, first_col_header = TRUE){
  # Table container
  shiny::tags$table(
    class = "djprTable",
    # Header row
    shiny::tags$thead(
      shiny::tags$tr(lapply(colnames(df), function(x) shiny::tags$th(scope = "col", x)))
    ),
    # Table body
    shiny::tags$tbody(
      apply(df, 1, function(x) {
        shiny::tags$tr(
          c(
            list(
              if(first_col_header) {
                shiny::tags$th(scope = "row", x[[1]])
              } else {
                  shiny::tags$td(x[[1]])
                }
              ),
            lapply(x[2:length(x)], function(y) shiny::tags$td(y))
          )
        )
      }
      )
    )
  )
}





highcharts_launchpad_goods <- function(
    data  = merch,
    dates = c(merch_dates$max - months(36), merch_dates$max),
    top   = 5
){

  past_12_months <- merch_dates$max - months(12)

  top_5_code <- data %>%
    dplyr::filter(
      .data$country_dest == "Total",
      .data$origin == "Victoria",
      nchar(.data$sitc_code) == 3,
      .data$date >= past_12_months,
      .data$sitc != "Total",
      .data$sitc_code != "988" #confidential items
    ) %>%
    dplyr::group_by(sitc_code) %>%
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(.data$value, n = top) %>%
    dplyr::select(.data$sitc_code) %>%
    dplyr::collect() %>%
    dplyr::pull()

  level_3_data <- data %>%
    dplyr::filter(
      .data$country_dest == "Total",
      .data$origin == "Victoria",
      .data$sitc_code %in% !!top_5_code,
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(sitc) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(value = slider::slide_mean(value, before = 11L) * 1000) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      sitc_shrink = sitc %>%
        str_remove_all("\\(.+\\)|and other.+|, fresh, chilled .+") %>%
        str_squish(),
      level_2 = substr(sitc_code, 1, 2)
    )

  level_3_data %>%
    dplyr::filter(
      .data$date >= dates[1],
      .data$date <= dates[2]
    ) %>%
    highcharter::hchart(
      "line",
      highcharter::hcaes(x = date, y = round(value, 0), group = sitc_shrink),
      color = djprtheme::djpr_pal(top),
      marker = list(enabled = F)
      ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_xAxis(title = list(enabled = FALSE)) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}"),
      tickAmount = 6
      ) %>%
    highcharter::hc_add_dependency("plugins/series-label.js") %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "Victorias top 5 exports (SITC level 3)"
      ) %>%
    highcharter::hc_title(text = "Top 5 goods exports from Victoria") %>%
    highcharter::hc_subtitle(text = "Top SITC level 3 exports by 12-month total exports, smoothed with 12-month rolling average") %>%
    highcharter::hc_caption(
      text = paste0(
        "Source: ABS.Stat Merchandise Exports by Commodity (latest data is from ",
        format(merch_dates$max, "%B %Y"),
        ")."
        )
      )
}

highcharts_launchpad_services <- function(
    data  = supp_cy,
    top   = 5
){

  vic_tables <- c(
    "Table 3.2 International Trade in Services, Credits, State by Calendar Year, $m - Vic. (a)",
    "Table 4.2 International Trade in Services, Debits, State by Calendar Year, $m - Vic. (a)"
  )

  avail_years <- data %>%
    dplyr::filter(abs_series %in% !!vic_tables) %>%
    dplyr::select(year) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull() %>%
    as.numeric() %>%
    sort()

  filter_years <- avail_years[length(avail_years):(length(avail_years) - 2)] %>%
    as.character()

  data <- data %>%
    dplyr::filter(
      abs_series %in% !!vic_tables,
      year %in% !!filter_years
    ) %>%
    dplyr::select(flow = abs_series, date = year, service = item, value) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      flow = dplyr::recode(
        flow,
        "Table 3.2 International Trade in Services, Credits, State by Calendar Year, $m - Vic. (a)" = "Import",
        "Table 4.2 International Trade in Services, Debits, State by Calendar Year, $m - Vic. (a)" = "Export"
      ),
      value = abs(value),
      date = as.Date(paste0(date, "-12-31"))
    ) %>% count(service) %>% View()


  top_5_code <- data %>%
    dplyr::filter(
      .data$country_dest == "Total",
      .data$origin == "Victoria",
      nchar(.data$sitc_code) == 3,
      .data$date >= past_12_months,
      .data$sitc != "Total",
      .data$sitc_code != "988" #confidential items
    ) %>%
    dplyr::group_by(sitc_code) %>%
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(.data$value, n = top) %>%
    dplyr::select(.data$sitc_code) %>%
    dplyr::collect() %>%
    dplyr::pull()

  level_3_data <- data %>%
    dplyr::filter(
      .data$country_dest == "Total",
      .data$origin == "Victoria",
      .data$sitc_code %in% !!top_5_code,
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(sitc) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(value = slider::slide_mean(value, before = 11L) * 1000) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      sitc_shrink = sitc %>%
        str_remove_all("\\(.+\\)|and other.+|, fresh, chilled .+") %>%
        str_squish(),
      level_2 = substr(sitc_code, 1, 2)
    )

  level_3_data %>%
    dplyr::filter(
      .data$date >= dates[1],
      .data$date <= dates[2]
    ) %>%
    highcharter::hchart(
      "line",
      highcharter::hcaes(x = date, y = round(value, 0), group = sitc_shrink),
      color = djprtheme::djpr_pal(top),
      marker = list(enabled = F)
    ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_xAxis(title = list(enabled = FALSE)) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}"),
      tickAmount = 6
    ) %>%
    highcharter::hc_add_dependency("plugins/series-label.js") %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "Victorias top 5 exports (SITC level 3)"
    ) %>%
    highcharter::hc_title(text = "Top 5 goods exports from Victoria") %>%
    highcharter::hc_subtitle(text = "Top SITC level 3 exports by 12-month total exports, smoothed with 12-month rolling average") %>%
    highcharter::hc_caption(
      text = paste0(
        "Source: ABS.Stat Merchandise Exports by Commodity (latest data is from ",
        format(merch_dates$max, "%B %Y"),
        ")."
      )
    )
}


table_countries <- function(exports = merch, imports = merch_imp){

  past_12_months <- merch_dates$max - months(12)

  top_exp <- exports %>%
    dplyr::filter(
      .data$country_dest != "Total",
      .data$origin == "Victoria",
      .data$date >= past_12_months,
      .data$sitc == "Total"
    ) %>%
    dplyr::group_by(country_dest) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(.data$value, n = 5) %>%
    dplyr::select(.data$country_dest) %>%
    dplyr::collect() %>%
    dplyr::pull()

  top_imp <- imports %>%
    dplyr::filter(
      .data$country_origin != "Total",
      .data$dest == "Victoria",
      .data$date >= past_12_months,
      .data$sitc == "Total"
    ) %>%
    dplyr::group_by(country_origin) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(.data$value, n = 5) %>%
    dplyr::select(.data$country_origin) %>%
    dplyr::collect() %>%
    dplyr::pull()

  exp <- exports %>%
    dplyr::filter(
      .data$country_dest != "Total",
      .data$origin == "Victoria",
      .data$date >= past_12_months,
      .data$sitc == "Total",
      .data$country_dest %in% top_exp
    ) %>%
    dplyr::arrange(country_dest, date) %>%
    dplyr::collect() %>%
    dplyr::group_by(country_dest) %>%
    dplyr::summarise(
      `Current monthly` = dplyr::last(value) * 1000,
      `One month change` = (dplyr::last(value) - dplyr::nth(value, -2)) / dplyr::nth(value, -2),
      `Three month change` = (dplyr::last(value) - dplyr::nth(value, -4)) / dplyr::nth(value, -4),
      `Twelve month change` = (dplyr::last(value) - dplyr::nth(value, -13)) / dplyr::nth(value, -13)
      ) %>%
    dplyr::arrange(dplyr::desc(`Current monthly`)) %>%
    dplyr::mutate(
      `Current monthly` = scales::dollar(`Current monthly`, accuracy = 1, scale = 1/1e06, suffix = "m"),
      dplyr::across(dplyr::contains("change"), ~scales::percent(., accuracy = 1)),
      country_dest = stringr::str_remove_all(country_dest, "\\(.+\\)") %>%
        stringr::str_squish() %>%
        stringr::str_replace_all("United States of America", "USA") %>%
        paste0("\t", .)
      ) %>%
    dplyr::rename(country = country_dest)

  imp <- imports %>%
    dplyr::filter(
      .data$country_origin != "Total",
      .data$dest == "Victoria",
      .data$date >= past_12_months,
      .data$sitc == "Total",
      .data$country_origin %in% top_imp
    ) %>%
    dplyr::arrange(country_origin, date) %>%
    dplyr::collect() %>%
    dplyr::group_by(country_origin) %>%
    dplyr::summarise(
      `Current monthly` = dplyr::last(value) * 1000,
      `One month change` = (dplyr::last(value) - dplyr::nth(value, -2)) / dplyr::nth(value, -2),
      `Three month change` = (dplyr::last(value) - dplyr::nth(value, -4)) / dplyr::nth(value, -4),
      `Twelve month change` = (dplyr::last(value) - dplyr::nth(value, -13)) / dplyr::nth(value, -13)
    ) %>%
    dplyr::arrange(dplyr::desc(`Current monthly`)) %>%
    dplyr::mutate(
      `Current monthly` = scales::dollar(`Current monthly`, accuracy = 1, scale = 1/1e06, suffix = "m"),
      dplyr::across(dplyr::contains("change"), ~scales::percent(., accuracy = 1)),
      country_origin = stringr::str_remove_all(country_origin, "\\(.+\\)") %>%
        stringr::str_squish() %>%
        stringr::str_replace_all("United States of America", "USA") %>%
        paste0("\t", .)
    ) %>%
    dplyr::rename(country = country_origin)

  exp_header <- as.list(c("Exports", rep("", 4)))
  imp_header <- as.list(c("Imports", rep("", 4)))

  out <- rbind(exp_header, exp, imp_header, imp) %>%
    djpr_table()

  return(out)

}







