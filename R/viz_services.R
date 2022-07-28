highcarts_service_compositon <- function(data = service_trade){

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
