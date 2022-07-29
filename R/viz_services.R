highcharts_service_compositon <- function(data = service_trade){

  level_1 <- data %>%
    filter(level == 1, date == max(date), state == "Vic.", flow == "Export") %>%
    select(date, level_1, value) %>%
    collect()

  level_3 <- data %>%
    filter(level == 3, date == max(date), state == "Vic.", flow == "Export") %>%
    select(id = level_2, name = service, y = value) %>%
    collect() %>%
    group_nest(id) %>%
    mutate(
      type = "pie",
      data = purrr::map(data, list_parse)
    )

  level_2 <- data %>%
    filter(level == 2, date == max(date), state == "Vic.", flow == "Export") %>%
    select(id = level_1, name = level_2, y = value, drilldown = level_2) %>%
    collect() %>%
    group_nest(id) %>%
    mutate(
      name = id,
      type = "pie",
      data = purrr::map(data, list_parse)
    ) %>%
    bind_rows(level_3)

  max_date <- max(level_1$date)

  date_text <- if(format(max_date, "%m") == "12"){
    format(max_date, "%Y")
  } else {
    paste0(
      as.integer(format(max_date, "%Y")) - 1,
      "-",
      format(max_date, "%y")
      )
  }

  level_1 <- level_1 %>% select(-date)

  level_1 %>%
    hchart("pie", hcaes(name = level_1, y = value, drilldown = level_1), name = "Total exports") %>%
    highcharter::hc_title(text = paste(date_text, "service trade compositon")) %>%
    highcharter::hc_subtitle(text = "Annual value of services exported $AUD") %>%
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list_parse(level_2)
    ) %>%
    djpr_highcharts() %>%
    highcharter::hc_caption(
      text = paste0(
        "Source: ABS International Trade: Supplementary Information (latest data is from ",
        date_text,
        ")."
      )
    ) %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "Victorias service exports"
    ) %>%
    highcharter::hc_tooltip(valuePrefix = "$") %>%
    hc_size(
      height = 650
    )
}


service_category_list <- function(data = service_trade){

  omit <- function(x){
    na.omit(x[x != ""])
  }

  v_li <- function(x){
    ifelse(is.na(x), as.character(NA), paste0("<li>", x, "</li>"))
  }

  v_ul <- function(x){
    if(all(is.na(x))) return(as.character(NA))
    if(all(x == "")) return(as.character(NA))
    out <- paste0(omit(x), collapse = "")
    paste0("<ul>", out, "</ul>")
  }


  data %>%
    filter(date == max(date)) %>%
    select(service, level, level_1, level_2, value) %>%
    distinct() %>%
    collect() %>%
    arrange(desc(value)) %>%
    mutate(level_3 = ifelse(level == 3, service, as.character(NA))) %>%
    select(-service, -level, -value) %>%
    distinct() %>%
    mutate(across(everything(), v_li)) %>%
    group_by(level_1, level_2) %>%
    summarise(level_3 = v_ul(level_3)) %>%
    ungroup() %>%
    tidyr::unite(level_2, c(level_2, level_3), na.rm = TRUE, sep ="") %>%
    group_by(level_1) %>%
    summarise(level_2 = v_ul(level_2)) %>%
    ungroup() %>%
    tidyr::unite(level_1, c(level_1, level_2), na.rm = TRUE, sep ="") %>%
    pull() %>%
    v_ul() %>%
    HTML()

}



highcharts_service_state_comp <- function(
    data = service_trade,
    product = "Education travel",
    period = "Calendar Year"
    ){

  data <- data %>%
    filter(
      flow == "Export",
      service == !!product,
      state != "Aust.",
      period == !!period
    ) %>%
    select(state, date, value) %>%
    collect()

highchart(type = "stock") %>%
  hc_add_series(data, "spline", hcaes(x=date, y = value, group = state)) %>%
  highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
  highcharter::hc_xAxis(
    title = list(enabled = FALSE),
    accessibility = list(
      description = sprintf(
        'Date from %s to %s',
        format(min(data$date), "%B %Y"),
        format(max(data$date), "%B %Y")
      )
    )
  ) %>%
  highcharter::hc_yAxis(
    title = list(text = "Exports"),
    labels = list(format = "${text}"),
    tickAmount = 6,
    accessibility = list(description = "Exports in Australian dollars")
  ) %>%
  highcharter::hc_add_dependency("plugins/series-label.js") %>%
  highcharter::hc_add_dependency("plugins/accessibility.js") %>%
  highcharter::hc_exporting(
    enabled = TRUE,
    filename = paste(product, "exports")
  ) %>%
  highcharter::hc_title(text = paste(product, "exports comparison")) %>%
  highcharter::hc_subtitle(text = "Export volume by Australian state $AUD") %>%
  highcharter::hc_caption(
    text = paste0(
      "Source: ABS International Trade: Supplementary Information (latest data is from ",
      format(max(data$date), "%Y"),
      ")."
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
  djpr_highcharts() %>%
  hc_elementId("service_state_comp")
}

update_service_state_comp <- function(
    product,
    states = c("Qld", "WA", "ACT", "Vic.", "NSW", "NT", "Aust.", "SA", "Tas."),
    data = service_trade,
    proxy_id = "service_state_comp",
    period = "Calendar Year"
    ){

  data <- data %>%
    filter(
      flow == "Export",
      service == !!product,
      state %in% states,
      period == !!period
    ) %>%
    select(state, date, value) %>%
    collect()

  highchartProxy(proxy_id) %>%
    hcpxy_remove_series(all = TRUE)

  highchartProxy(proxy_id) %>%
    hcpxy_add_series(data, "spline", hcaes(x = date, y = value, group = state))

  highchartProxy(proxy_id) %>%
    hcpxy_update(title = list(text = paste(product, "exports comparison")))

}


