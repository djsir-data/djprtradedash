
# Services
badge_services <- function(...){
  div(
    onclick = if(format(service_dates$max, "%m") == "12"){
      "window.open('https://www.abs.gov.au/statistics/economy/international-trade/international-trade-supplementary-information-calendar-year/latest-release','mywindow');"
    } else {
      "window.open('https://www.abs.gov.au/statistics/economy/international-trade/international-trade-supplementary-information-financial-year/latest-release','mywindow');"
    },
    class = "sourceBadge",
    style = "display: table;",
    div(
      style = "display: table-cell;",
      "Services"
    ),
    div(
      style = "display: table-cell;",
      if(format(service_dates$max, "%m") == "12"){
        format(service_dates$max, "%Y")
      } else {
        paste0(
          as.numeric(format(service_dates$max, "%y")) - 1,
          "-",
          format(service_dates$max, "%y"),
          " FY"
        )
      } %>%
        stringr::str_replace_all("[:blank:]", "&nbsp;") %>%
        stringr::str_replace_all("-", "&#8209;") %>%
        HTML(),
      br(),
      span(style = "font-size:8px", "Annual")
    )
  )
}

# goods

badge_goods <- function(...){
  div(
    onclick="window.open('https://explore.data.abs.gov.au/vis?fs[0]=Economy%2C0%7CInternational%20trade%23INTERNATIONAL_TRADE%23&pg=0&fc=Economy&df[ds]=ECONOMY_TOPICS&df[id]=MERCH_EXP&df[ag]=ABS&df[vs]=1.0.0&pd=2021-01%2C&dq=0%2B1%2B2%2B3%2B4%2B5%2B6%2B7%2B8%2B9%2BTOT.TOT.TOT.M&ly[cl]=TIME_PERIOD&ly[rw]=COMMODITY_SITC','mywindow');",
    class = "sourceBadge",
    style = "cursor: pointer;",
    div(
      style = "display: table-cell;",
      "Goods"
    ),
    div(
      style = "display: table-cell;",
      format(merch_dates$max, "%b %Y") %>%
        stringr::str_replace_all("[:blank:]", "&nbsp;") %>%
        HTML(),
      br(),
      span(style = "font-size:8px", "Monthly")
    )
  )
}


# BOP

badge_bop <- function(...){
  div(
    onclick="window.open('https://www.abs.gov.au/statistics/economy/international-trade/balance-payments-and-international-investment-position-australia/latest-release','mywindow');",
    class = "sourceBadge",
    style = "cursor: pointer;",
    div(
      style = "display: table-cell;",
      "BOP"
    ),
    div(
      style = "display: table-cell;",
      format(bop_dates$max, "%b %Y") %>%
        stringr::str_replace_all("[:blank:]", "&nbsp;") %>%
        HTML(),
      br(),
      span(style = "font-size:8px", "Quarterly")
    )
  )
}
