# Country-based merch import and exports

table_merch_country <- function(
  flow = "exports",
  n = 5,
  data_exp = merch,
  data_imp = merch_imp
){
  # input check
  stopifnot(flow %in% c("exports", "imports"))

  # Select required data based on import and export
  data <- if(flow == "exports"){data_exp} else {merch_imp}

  # Extract correct column names depending on dataset
  foriegn_col <- intersect(c("country_dest", "country_origin"), colnames(data))
  domestic_col <- intersect(c("dest", "origin"), colnames(data))

  # Get current top trading partners
  top_traders <- data %>%
    filter(
      !!sym(domestic_col) == "Victoria",
      !!sym(foriegn_col) != "Total",
      sitc == "Total",
      date == max(date)
      ) %>%
    slice_max(value, n = n) %>%
    select(!!sym(foriegn_col)) %>%
    distinct() %>%
    collect() %>%
    pull()

  # Create date vector
  date_select <- c(
    merch_dates$max,
    merch_dates$max - months(1),
    merch_dates$max - months(3),
    merch_dates$max - months(12)
  )

  names(date_select) <- c(
    paste(format(date_select[1], "%B"), flow),
    "One Month change",
    "One Quarter change",
    "One Year change"
  )

  # Collect data
  data <- data %>%
    filter(
      !!sym(domestic_col) == "Victoria",
      !!sym(foriegn_col) %in% !!top_traders,
      date %in% !!date_select,
      sitc == "Total"
    ) %>%
    select(date, partner = !!sym(foriegn_col), value) %>%
    collect() %>%
    mutate(value = value / 1000)


  # Tidy & generate features
  summary <- data %>%
    arrange(desc(date), desc(value)) %>%
    group_by(partner) %>%
    mutate(
      abs_diff = first(value) - value,
      rel_diff = round((first(value) - value) / value, 2)
    ) %>%
    ungroup() %>%
    mutate(
      abs_diff = format_table_num(abs_diff, suffix = "m"),
      rel_diff = format_table_pc(rel_diff),
      value    = format_table_num(value, suffix = "m", plus_neg = F),
      date = names(date_select)[match(date, date_select)],
      partner = partner %>%
        stringr::str_remove_all("\\(.+\\)") %>%
        stringr::str_replace("United States of America", "USA") %>%
        stringr::str_replace("United Kingdom, Channel Islands and Isle of Man, nfd", "UK") %>%
        stringr::str_squish()
    )


  # Generate summary table
  summary_current_cols <- summary %>%
    filter(date == names(date_select)[1]) %>%
    select(partner, value)

  col_order <- paste0(
    rep(names(date_select)[2:4], each = 2),
    "_",
    rep(c("abs_diff", "rel_diff"), 3)
    )

  summary_change_cols <- summary %>%
    select(-value) %>%
    filter(date != names(date_select)[1]) %>%
    tidyr::pivot_wider(
      names_from = date,
      values_from = c("abs_diff", "rel_diff"),
      names_glue = "{date}_{.value}"
      ) %>%
    relocate(col_order, .after = partner)

  summary <- summary_current_cols %>%
    full_join(summary_change_cols, by = "partner")



  # Create header
  header <- tags$thead(
    tags$tr(
      tags$th("Partner", rowspan = "2", scope = "col"),
      tags$th(names(date_select)[1], rowspan = "2", scope = "col"),
      lapply(
        names(date_select)[2:4],
        shiny::tags$th,
        scope = "col",
        colspan = "2"
      )
    ),
    tags$tr(
      lapply(
        rep(c("$", "%"), 3),
        shiny::tags$th,
        scope = "col"
      )
    )
  )


  # Create body
  body <- shiny::tags$tbody(
    apply(summary, 1, function(x) {
      shiny::tags$tr(
        c(
          list(shiny::tags$th(scope = "row", x[[1]])),
          lapply(x[2:length(x)], function(y) shiny::tags$td(y))
        )
      )
    }
    )
  )

  # Return
  return(
    tags$table(class = "djprTable", header, body)
  )

}



# Product-based merch import and exports

table_merch_product <- function(
    flow = "exports",
    n = 5,
    sitc_level = 3,
    data_exp = merch,
    data_imp = merch_imp
){
  # input check
  stopifnot(flow %in% c("exports", "imports"))

  # Select required data based on import and export
  data <- if(flow == "exports"){data_exp} else {merch_imp}

  # Extract correct column names depending on dataset
  foriegn_col <- intersect(c("country_dest", "country_origin"), colnames(data))
  domestic_col <- intersect(c("dest", "origin"), colnames(data))

  # Get current top trading partners
  top_traders <- data %>%
    filter(
      !!sym(domestic_col) == "Victoria",
      !!sym(foriegn_col) == "Total",
      sitc != "Total",
      substr(sitc_code, 1, 1) != "9",
      nchar(sitc_code) == !!sitc_level,
      date == max(date)
    ) %>%
    slice_max(value, n = n) %>%
    select(sitc) %>%
    distinct() %>%
    collect() %>%
    pull()

  # Create date vector
  date_select <- c(
    merch_dates$max,
    merch_dates$max - months(1),
    merch_dates$max - months(3),
    merch_dates$max - months(12)
  )

  names(date_select) <- c(
    paste(format(date_select[1], "%B"), flow),
    "One Month change",
    "One Quarter change",
    "One Year change"
  )

  # Collect data
  data <- data %>%
    filter(
      !!sym(domestic_col) == "Victoria",
      !!sym(foriegn_col) == "Total",
      date %in% !!date_select,
      sitc %in% !!top_traders,
      nchar(sitc_code) == !!sitc_level
    ) %>%
    select(date, sitc, sitc_code, value) %>%
    collect() %>%
    mutate(value = value / 1000)


  # Tidy & generate features
  summary <- data %>%
    arrange(desc(date), desc(value)) %>%
    group_by(sitc) %>%
    mutate(
      abs_diff = first(value) - value,
      rel_diff = round((first(value) - value) / value, 2)
    ) %>%
    ungroup() %>%
    mutate(
      abs_diff = format_table_num(abs_diff, suffix = "m"),
      rel_diff = format_table_pc(rel_diff),
      value    = format_table_num(value, suffix = "m", plus_neg = F),
      date = names(date_select)[match(date, date_select)],
      sitc = sitc %>%
        stringr::str_remove_all("\\(.+\\)|;.+") %>%
        stringr::str_squish()
    )


  # Generate summary table
  summary_current_cols <- summary %>%
    filter(date == names(date_select)[1]) %>%
    select(sitc, sitc_code, value)

  col_order <- paste0(
    rep(names(date_select)[2:4], each = 2),
    "_",
    rep(c("abs_diff", "rel_diff"), 3)
  )

  summary_change_cols <- summary %>%
    select(-value) %>%
    filter(date != names(date_select)[1]) %>%
    tidyr::pivot_wider(
      names_from = date,
      values_from = c("abs_diff", "rel_diff"),
      names_glue = "{date}_{.value}"
    ) %>%
    relocate(col_order, .after = sitc_code)

  summary <- summary_current_cols %>%
    full_join(summary_change_cols, by = c("sitc", "sitc_code"))



  # Create header
  header <- tags$thead(
    tags$tr(
      tags$th("Product", rowspan = "2", scope = "col"),
      tags$th("SITC", rowspan = "2", scope = "col"),
      tags$th(names(date_select)[1], rowspan = "2", scope = "col"),
      lapply(
        names(date_select)[2:4],
        shiny::tags$th,
        scope = "col",
        colspan = "2"
      )
    ),
    tags$tr(
      lapply(
        rep(c("$", "%"), 3),
        shiny::tags$th,
        scope = "col"
      )
    )
  )


  # Create body
  body <- shiny::tags$tbody(
    apply(summary, 1, function(x) {
      shiny::tags$tr(
        c(
          list(shiny::tags$th(scope = "row", x[[1]])),
          lapply(x[2:length(x)], function(y) shiny::tags$td(y))
        )
      )
    }
    )
  )

  # Return
  return(
    tags$table(class = "djprTable", header, body)
  )

}



# BOP summary table
table_bop <- function(data = bop){

  # Create date vector
  date_select <- c(
    bop_dates$max,
    bop_dates$max - months(3),
    bop_dates$max - months(12),
    as.Date("2019-12-01")
  )

  names(date_select) <- c(
    paste(format(date_select[1], "%B"), "quarter trade"),
    "One Quarter change",
    "One Year change",
    "Change since COVID"
  )

  # Collect data
  data <- data %>%
    filter(
      state == "Victoria",
      indicator == "Chain Volume Measures",
      date %in% !!date_select
    ) %>%
    select(exports_imports, goods_services, date, value) %>%
    collect() %>%
    mutate(value = value / 1000)

  # Tidy & generate features
  summary <- data %>%
    arrange(desc(date), desc(value)) %>%
    group_by(exports_imports, goods_services) %>%
    mutate(
      abs_diff = first(value) - value,
      rel_diff = round((first(value) - value) / value, 2)
    ) %>%
    ungroup() %>%
    mutate(
      abs_diff = format_table_num(abs_diff, suffix = "b"),
      rel_diff = format_table_pc(rel_diff),
      value    = format_table_num(value, suffix = "b", plus_neg = F),
      date = names(date_select)[match(date, date_select)]
    )

  # Generate summary table
  summary_current_cols <- summary %>%
    filter(date == names(date_select)[1]) %>%
    select(exports_imports, goods_services, value)

  col_order <- paste0(
    rep(names(date_select)[2:4], each = 2),
    "_",
    rep(c("abs_diff", "rel_diff"), 3)
  )

  summary_change_cols <- summary %>%
    select(-value) %>%
    filter(date != names(date_select)[1]) %>%
    tidyr::pivot_wider(
      names_from = date,
      values_from = c("abs_diff", "rel_diff"),
      names_glue = "{date}_{.value}"
    ) %>%
    relocate(col_order, .after = goods_services)

  summary <- summary_current_cols %>%
    full_join(
      summary_change_cols,
      by = c("exports_imports", "goods_services")
      ) %>%
    mutate(
      goods_services = factor(
        goods_services,
        levels = c("Goods and Services", "Goods", "Services")
        )
      ) %>%
    arrange(exports_imports, goods_services) %>%
    mutate(
      series = ifelse(
        goods_services == "Goods and Services",
        exports_imports,
        paste(goods_services, tolower(exports_imports))
        ),
      .before = exports_imports
      ) %>%
    select(-exports_imports, -goods_services)

  # Create header
  header <- tags$thead(
    tags$tr(
      tags$th("Trade", rowspan = "2", scope = "col"),
      tags$th(names(date_select)[1], rowspan = "2", scope = "col"),
      lapply(
        names(date_select)[2:4],
        shiny::tags$th,
        scope = "col",
        colspan = "2"
      )
    ),
    tags$tr(
      lapply(
        rep(c("$", "%"), 3),
        shiny::tags$th,
        scope = "col"
      )
    )
  )

  # Create body
  body <- shiny::tags$tbody(
    apply(summary, 1, function(x) {
      if(x[[1]] %in% c("Exports", "Imports")){
        shiny::tags$tr(
          class = "djprTableHeading",
          c(
            list(shiny::tags$th(scope = "row", x[[1]])),
            lapply(x[2:length(x)], function(y) shiny::tags$td(y))
          )
        )
      } else{
        shiny::tags$tr(
          c(
            list(shiny::tags$th(scope = "row", x[[1]])),
            lapply(x[2:length(x)], function(y) shiny::tags$td(y))
          )
        )
      }
    }
    )
  )

  # Return
  return(
    tags$table(class = "djprTable", header, body)
  )

}



# Headline goods chart
highcharts_launchpad_goods <- function(
    data  = merch,
    dates = c(merch_dates$max - months(36), merch_dates$max),
    top   = 5,
    chart_type = "spline",
    sitc_level = 3
){

  past_12_months <- merch_dates$max - months(12)

  top_5_code <- data %>%
    dplyr::filter(
      country_dest == "Total",
      origin == "Victoria",
      nchar(sitc_code) == !!sitc_level,
      date >= past_12_months,
      sitc != "Total",
      substr(sitc_code, 1, 1) != "9" #confidential items and misc
    ) %>%
    dplyr::group_by(sitc_code) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(value, n = top) %>%
    dplyr::select(sitc_code) %>%
    dplyr::collect() %>%
    dplyr::pull()

  level_3_data <- data %>%
    dplyr::filter(
      country_dest == "Total",
      origin == "Victoria",
      sitc_code %in% !!top_5_code,
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(sitc) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      value = slider::slide_sum(value, before = 11L) * 1000
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      sitc_shrink = sitc %>%
        stringr::str_remove_all(
          "\\(.+\\)|and other.+|, fresh, chilled .+|, fats and waxes| and related products, nes|, inedible, except fuels| and live animals| and transport equipment|, lubricants and related materials| articles"
        ) %>%
        stringr::str_squish(),
      level_2 = substr(sitc_code, 1, 2)
    )

  level_3_data %>%
    dplyr::filter(
      date >= dates[1],
      date <= dates[2]
    ) %>%
    highcharter::hchart(
      chart_type,
      highcharter::hcaes(x = date, y = round(value, 0), group = sitc_shrink),
      marker = list(enabled = F)
    ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_xAxis(
      title = list(enabled = FALSE),
      accessibility = list(
        description = sprintf(
          'Date from %s to %s',
          format(dates[1], "%B %Y"),
          format(dates[2], "%B %Y")
        )
      )
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}"),
      tickAmount = 6,
      accessibility = list(description = "Exports in Australian dollars")
    ) %>%
    #highcharter::hc_add_dependency("plugins/series-label.js") %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "Victorias good exports"
    ) %>%
    highcharter::hc_title(text = "Goods exports from Victoria") %>%
    highcharter::hc_subtitle(text = "Year to date goods exports of level 1 SITC classifications $AUD") %>%
    highcharter::hc_caption(
      text = paste0(
        "Source: ABS.Stat Merchandise Exports by Commodity."
      )
    ) %>%
    highcharter::hc_tooltip(
      dateTimeLabelFormats = list(day = "%b %Y"),
      valuePrefix = "$"
    ) %>%
    djpr_highcharts()
}


# Headline services chart
highcharts_launchpad_services <- function(
    data    = service_trade,
    n_years = 4,
    period  = "Calendar Year",
    chart_type = "spline"
){

  filter_years <- data %>%
    dplyr::filter(period == !!period) %>%
    dplyr::select(date) %>%
    dplyr::distinct() %>%
    dplyr::slice_max(date, n = n_years) %>%
    dplyr::collect() %>%
    dplyr::pull()

  data <- data %>%
    dplyr::filter(
      flow == "Export",
      state == "Vic.",
      period == !!period,
      date %in% !!filter_years
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      keep = level == 1,
      keep = ifelse(level_1 == "Travel" & level > 2, T, keep),
      keep = ifelse(service == "Travel", F, keep)
    ) %>%
    dplyr::filter(keep == TRUE) %>%
    dplyr::select(date, value, service)

  data %>%
    highcharter::hchart(
      chart_type,
      highcharter::hcaes(x = date, y = value, group = service),
      marker = list(enabled = F)
    ) %>%
    highcharter::hc_plotOptions(
      series = list(label = list(enabled = TRUE)),
      area = list(stacking = 'normal')
    ) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_tooltip(
      dateTimeLabelFormats = list(day = "%Y"),
      valuePrefix = "$"
    ) %>%
    highcharter::hc_xAxis(title = list(enabled = FALSE)) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}"),
      tickAmount = 6
    ) %>%
    #highcharter::hc_add_dependency("plugins/series-label.js") %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "Victorias service exports"
    ) %>%
    highcharter::hc_title(text = "Services exports from Victoria") %>%
    highcharter::hc_subtitle(text = "Annual value of services exported $AUD") %>%
    highcharter::hc_caption(
      text = paste0(
        "Source: ABS International Trade: Supplementary Information."
      )
    ) %>%
    djpr_highcharts()
}


# Top country exports/imports
table_countries <- function(exports = merch, imports = merch_imp){

  # Determine last 12 months of data
  past_12_months <- merch_dates$max - months(12)

  # Get the top export countries by sum of 12 month value
  top_exp <- exports %>%
    dplyr::filter(
      country_dest != "Total",
      origin == "Victoria",
      date >= past_12_months,
      sitc == "Total"
    ) %>%
    dplyr::group_by(country_dest) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(value, n = 5) %>%
    dplyr::select(country_dest) %>%
    dplyr::collect() %>%
    dplyr::pull()

  # Get the top import countries by sum of 12 month value
  top_imp <- imports %>%
    dplyr::filter(
      country_origin != "Total",
      dest == "Victoria",
      date >= past_12_months,
      sitc == "Total"
    ) %>%
    dplyr::group_by(country_origin) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(value, n = 5) %>%
    dplyr::select(country_origin) %>%
    dplyr::collect() %>%
    dplyr::pull()

  exp <- exports %>%
    dplyr::filter(
      country_dest != "Total",
      origin == "Victoria",
      date >= past_12_months,
      sitc == "Total",
      country_dest %in% top_exp
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
      country_origin != "Total",
      dest == "Victoria",
      date >= past_12_months,
      sitc == "Total",
      country_origin %in% top_imp
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


# Overall trade position
highcharts_bop_export_chart <- function(
    data = bop
) {

  df <- data %>%
    dplyr::filter(
      state == "Victoria",
      exports_imports == "Exports",
      indicator == "Chain Volume Measures"
    )

  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df <- df %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::mutate(goods_services = dplyr::if_else(goods_services == "Goods and Services", "Total", goods_services)) %>%
    dplyr::mutate(value = abs(value) * 1000000)


  latest_month <- format(max(df$date), "%B %Y")

  latest_change <- df %>%
    dplyr::filter(goods_services == "Total") %>%
    dplyr::mutate(change = value - dplyr::lag(value, 1)) %>%
    dplyr::filter(!is.na(change), date == max(date))



  title <-
    dplyr::case_when(
      abs(latest_change$change) < 10 ~ "Victoria's exports remained steady over the past quarter ",
      latest_change$change > 0 ~ paste0("Victoria's exports rose by ", dollar_stat(latest_change$change), " over the past quarter"),
      latest_change$change < 0 ~ paste0("Victoria's exports fell by ", dollar_stat(abs(latest_change$change)), " over the past quarter"),
      TRUE ~ "Victoria's exports over the past quarter"
    )

  caption <- paste0("Source: ABS Balance of Payment quarterly.</br> Note: Data seasonally Adjusted & Chain Volume Measures")

  highcharter::highchart(type = "stock") %>%
    highcharter::hc_add_series(
      df,
      "line",
      highcharter::hcaes(x = date, y = value, group = goods_services),
      label = list(enabled = TRUE)
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}")
    ) %>%
    #highcharter::hc_add_dependency("plugins/series-label.js") %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = "Victoria's exports of goods and services") %>%
    highcharter::hc_caption(text = caption) %>%
    highcharter::hc_rangeSelector(
      inputEnabled = F,
      selected = 1,
      buttons = list(
        list(
          type  = 'all',
          text  =  'All',
          title =  'View all'
        ),
        list(
          type  = 'year',
          count = 5,
          text  = '5y',
          title = 'View five years'
        ),
        list(
          type  = 'year',
          count = 3,
          text  = '3y',
          title = 'View three years years'
        ),
        list(
          type  = 'ytd',
          text  = 'YTD',
          title = 'View year to date'
        )
      )
    ) %>%
    highcharter::hc_navigator(series = list(label = list(enabled = FALSE))) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    highcharter::hc_tooltip(
      dateTimeLabelFormats = list(day = "%b %Y"),
      valuePrefix = "$"
    ) %>%
    djpr_highcharts()

}



# Rising goods chart
highcharts_rising_goods <- function(
    data  = merch,
    top   = 5,
    chart_type = "spline",
    sitc_level = 3,
    min_annual_value = 1e06
){

  past_12_months <- merch_dates$max - months(12)

  top_5_code <- data %>%
    dplyr::filter(
      country_dest == "Total",
      origin == "Victoria",
      nchar(sitc_code) == !!sitc_level,
      sitc != "Total",
      substr(sitc_code, 1, 1) != "9" #confidential items and misc
    ) %>%
    dplyr::arrange(sitc_code, date) %>%
    dplyr::collect() %>%
    dplyr::group_by(sitc_code) %>%
    dplyr::mutate(year_on_year_growth = (value - dplyr::lag(value, 12)) / dplyr::lag(value, 12)) %>%
    dplyr::filter(date >= !!past_12_months) %>%
    dplyr::summarise(mean_growth = mean(year_on_year_growth), value = sum(value)) %>%
    dplyr::filter(value >= !!min_annual_value) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(mean_growth, n = top) %>%
    dplyr::select(sitc_code) %>%
    dplyr::pull()

  level_3_data <- data %>%
    dplyr::filter(
      country_dest == "Total",
      origin == "Victoria",
      sitc_code %in% !!top_5_code,
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(sitc) %>%
    dplyr::mutate(value = slider::slide_mean(value, before = 11)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      sitc_shrink = sitc %>%
        stringr::str_remove_all(
          "\\(.+\\)|and other.+|, fresh, chilled .+|, fats and waxes| and related products, nes|, inedible, except fuels| and live animals| and transport equipment|, lubricants and related materials| articles| of a kind used for the extraction of soft fixed vegetable oils|;.+"
        ) %>%
        stringr::str_squish()
    ) %>%
    dplyr::select(sitc_shrink, date, value) %>%
    dplyr::arrange(date, sitc_shrink) %>%
    dplyr::distinct()


  highcharter::highchart(type = "stock") %>%
    highcharter::hc_add_series(
      level_3_data,
      chart_type,
      highcharter::hcaes(x = date, y = round(value, 0), group = sitc_shrink),
      marker = list(enabled = F)
    ) %>%
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    highcharter::hc_xAxis(
      title = list(enabled = FALSE),
      accessibility = list(
        description = sprintf(
          'Date from %s to %s',
          format(min(level_3_data$date), "%B %Y"),
          format(max(level_3_data$date), "%B %Y")
        )
      )
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = "Exports"),
      labels = list(format = "${text}"),
      tickAmount = 6,
      accessibility = list(description = "Exports in Australian dollars")
    ) %>%
    #highcharter::hc_add_dependency("plugins/series-label.js") %>%
    #highcharter::hc_add_dependency("plugins/accessibility.js") %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "Victorias good exports"
    ) %>%
    highcharter::hc_title(text = "Past year's top growing goods exports") %>%
    highcharter::hc_subtitle(text = "Top 5 SITC level 3 exports by average year-on-year growth $AUD") %>%
    highcharter::hc_caption(
      text = paste0(
        "Source: ABS.Stat Merchandise Exports by Commodity.</br>Note: Data smoothed using 12-month rolling average."
      )
    ) %>%
    highcharter::hc_rangeSelector(
      inputEnabled = F,
      selected = 1,
      buttons = list(
        list(
          type  = 'all',
          text  =  'All',
          title =  'View all'
        ),
        list(
          type  = 'year',
          count = 5,
          text  = '5y',
          title = 'View five years'
        ),
        list(
          type  = 'year',
          count = 3,
          text  = '3y',
          title = 'View three years years'
        ),
        list(
          type  = 'ytd',
          text  = 'YTD',
          title = 'View year to date'
        )
      )
    ) %>%
    highcharter::hc_navigator(series = list(label = list(enabled = FALSE))) %>%
    highcharter::hc_tooltip(
      dateTimeLabelFormats = list(day = "%b %Y"),
      valuePrefix = "$"
    ) %>%
    djpr_highcharts()
}




