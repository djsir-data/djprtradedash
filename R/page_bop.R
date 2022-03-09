page_bop <- function(...) {
  djpr_tab_panel(
    title = "Balance of Payments",
    h1(
      span("Balance of payments",
           style = "font-size: 40px; color: #1F1547; font-family: 'Roboto Slab'"
      )
    ),
    p(
      "Summary information on Victoria's quarterly trade",
      style = "font-size: 150%;font-style: italic;color: #A9A9A9"
      ),
    br(),
    br(),
    h2("Goods and Services", align = "center"),
    br(),
    djpr_plot_ui("good_services_chart"),
    br(),
    djpr_plot_ui("total_bop_bar_chart",
      interactive = FALSE
    ),
    br(),
    djpr_plot_ui("goods_export_import_line"),
    br(),
    djpr_plot_ui("vic_total_bop_cumul_line"),
    br(),
    br(),
    h2("Goods", align = "center"),
    br(),
    djpr_plot_ui("goods_bop_bar_chart",
      interactive = FALSE
    ),
    br(),
    djpr_plot_ui("good_trade_line_chart"),
    br(),
    djpr_plot_ui("NSW_Vic_goods_line_chart"),
    br(),
    br(),
    h2("Services", align = "center"),
    br(),
    djpr_plot_ui("service_bop_bar_chart",
      interactive = FALSE
    ),
    br(),
    djpr_plot_ui("services_trade_line_chart"),
    djpr_plot_ui("NSW_Vic_Services_line_chart"),
    br(),
    br(),
    h2("Balance of Trade", align = "center"),
    br(),
    djpr_plot_ui("trade_balance_line_chart")
  )
}
