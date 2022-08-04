page_bopUI <- function(...) {

  shiny::fluidPage(

    # Overview
    "Balance of Payments" %>%
      h2() %>% div(class = "inner", style = "background-color: #1F1547;") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      column(
        12,
        div(
          class = "box",
          readRDS("inst/bop_good_services_chart.rds")
        )
      )
    ),

    fluidRow(
      column(
        6,
        div(
          class = "box",
          readRDS("inst/bop_goods_export_import_line.rds")
        )
      ),
      column(
        6,
        div(
          class = "box",
          readRDS("inst/bop_total_bop_bar_chart.rds")
        )
      )
    ),


    # Goods
    "Goods" %>%
      h2() %>% div(class = "inner", style = "background-color: #1F1547;") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      column(
        6,
        div(
          class = "box",
          readRDS("inst/bop_goods_bop_bar_chart.rds")
        )
      ),
      column(
        6,
        div(
          class = "box",
          readRDS("inst/bop_good_trade_line_chart.rds")
        )
      )
    ),

    fluidRow(
      column(
        12,
        div(
          class = "box",
          readRDS("inst/bop_NSW_Vic_goods_line_chart.rds")
        )
      )
    ),

    # Services
    "Services" %>%
      h2() %>% div(class = "inner", style = "background-color: #1F1547;") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      column(
        6,
        div(
          class = "box",
          readRDS("inst/bop_service_bop_bar_chart.rds")
        )
      ),

      column(
        6,
        div(
          class = "box",
          readRDS("inst/bop_services_trade_line_chart.rds")
        )
      )
    ),

    fluidRow(
      column(
        12,
        div(
          class = "box",
          readRDS("inst/bop_NSW_Vic_Services_line_chart.rds")
        )
      )
    ),

    # Balance of trade
    "Balance of Trade" %>%
      h2() %>% div(class = "inner", style = "background-color: #1F1547;") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      column(
        12,
        div(
          class = "box",
          readRDS("inst/bop_trade_balance_line_chart.rds")
        )
      )
    )
  )
}

