page_bopUI <- function(...) {

  shiny::fluidPage(

    # Overview
    djprshiny::djpr_h2_box("Balance of Payments") %>% fluidRow(),

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
    djprshiny::djpr_h2_box("Goods") %>% fluidRow(),

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
    djprshiny::djpr_h2_box("Services") %>% fluidRow(),

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
    djprshiny::djpr_h2_box("Balance of Trade") %>% fluidRow(),

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

