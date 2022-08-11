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











# The table that shows the change in exports and imports of goods and services
launchpad_table_export_import <- function(data = bop) {

  if ('tbl_lazy' %in% class(data)) {
    data <- data %>%
      dplyr::collect() %>%
      dplyr::mutate(date = as.Date(date))
  }


  df <- data %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::filter(
      indicator == "Chain Volume Measures",
      state == "Victoria"
      ) %>%
    dplyr::mutate(value = abs(value))

  datelist <- df %>%
    dplyr::filter(date %in% c(
      max(date),
      max(date) - months(3),
      max(date) - months(12),
      as.Date("2019-12-01")
      )) %>%
    dplyr::select(date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(date))

  #clean table headings
  latest_date <- datelist$date[1]
  prev_qtr <- datelist$date[2]
  prev_year <- datelist$date[3]
  since_covid <- datelist$date[4]

  nice_latest_date <- format(latest_date, "%b %Y")
  nice_since_covid  <- format(since_covid, "%b %Y")
  nice_prev_qtr <- format(prev_qtr, "%b %Y")
  nice_prev_year <- format(prev_year, "%b %Y")

  since_since_covid <- paste0("Since ", nice_since_covid)
  since_prev_qtr <- paste0("Since ", nice_prev_qtr)
  since_prev_year <- paste0("Since ", nice_prev_year)

  df %>%
    dplyr::filter(date %in% datelist$date) %>%
    dplyr::group_by(exports_imports, goods_services) %>%
    dplyr::arrange(exports_imports, goods_services) %>%
    dplyr::mutate(
      prev_qtr_change = paste0(
        scales::comma(value - dplyr::lag(value,1), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(value,1)) / dplyr::lag(value,1)
          ),
        ")"
        ),
      prev_year_change = paste0(
        scales::comma(value - dplyr::lag(value, 2), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(value, 2)) / dplyr::lag(value, 2)
          ),
        ")"
        ),
      covid_change = paste0(
        scales::comma(value - dplyr::lag(value,3), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(value,3)) / dplyr::lag(value,3)
          ),
        ")"
        )
      ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(date == latest_date) %>%
    dplyr::arrange(exports_imports, dplyr::desc(value)) %>%
    dplyr::mutate(
      ` `   = paste(goods_services, exports_imports),
      value = scales::comma(value, accuracy = 1)
      ) %>%
    dplyr::select(
      -c(
        exports_imports,
        indicator,
        goods_services,
        state,
        date
      )
    ) %>%
    dplyr::rename(
      !!paste0(nice_latest_date) := value,
      !!paste0(nice_prev_qtr)    := prev_qtr_change,
      !!paste0(nice_prev_year)   := prev_year_change,
      !!paste0(nice_since_covid) := covid_change
      ) %>%
    dplyr::relocate(` `)
}

