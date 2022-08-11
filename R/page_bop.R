page_bopUI <- function(...) {

  shiny::fluidPage(

    # Overview
    "Balance of Payments" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      shinydashboard::box(
        width = 12,
        readRDS("inst/bop_good_services_chart.rds")
      ) %>% to_col_xl()
    ),

    fluidRow(
      shinydashboard::box(
        readRDS("inst/bop_goods_export_import_line.rds")
      ) %>% to_col_xl(),
      shinydashboard::box(
        readRDS("inst/bop_total_bop_bar_chart.rds")
      ) %>% to_col_xl()
    ),


    # Goods
    "Goods" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      shinydashboard::box(
        readRDS("inst/bop_goods_bop_bar_chart.rds")
      )%>% to_col_xl(),
      shinydashboard::box(
        readRDS("inst/bop_good_trade_line_chart.rds")
      )%>% to_col_xl()
    ),

    fluidRow(
      shinydashboard::box(
        width = 12,
        readRDS("inst/bop_NSW_Vic_goods_line_chart.rds")
      )%>% to_col_xl()
    ),

    # Services
    "Services" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      shinydashboard::box(
        readRDS("inst/bop_service_bop_bar_chart.rds")
      ) %>% to_col_xl(),

      shinydashboard::box(
        readRDS("inst/bop_services_trade_line_chart.rds")
      ) %>% to_col_xl()
    ),

    fluidRow(
      shinydashboard::box(
        width = 12,
        readRDS("inst/bop_NSW_Vic_Services_line_chart.rds")
      )%>% to_col_xl()
    ),

    # Balance of trade
    "Balance of Trade" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      shinydashboard::box(
        width = 12,
        readRDS("inst/bop_trade_balance_line_chart.rds")
      )%>% to_col_xl()
    )
  )
}

