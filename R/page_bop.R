page_bop <- function(...) {
  djpr_tab_panel(
    title = "Balance of Payments",
    h1("Key indicators"),
    paste0("This page contains Victoria's international transactions, typically quarterly or
    yearly, over a particular period.  It shows the sum of the transactions of those involving
    goods or services."),
    h2(br(), "Goods and Services"),
    djpr_plot_ui("good_services_import_chart"),
    br(),
    djpr_plot_ui("total_bop_bar_chart",
      interactive = FALSE
    ),
    br(),
    djpr_plot_ui("goods_export_import_line"),
    # br(),
    # djpr_plot_ui("vic_total_bop_cumul_line"),
    h2(br(), "Goods"),
    djpr_plot_ui("goods_bop_bar_chart",
      interactive = FALSE
    ),
    br(),
    djpr_plot_ui("good_trade_line_chart"),
    br(),
    djpr_plot_ui("NSW_Vic_goods_line_chart"),
    br(),
    h2(br(), "Services"),
    djpr_plot_ui("service_bop_bar_chart",
      interactive = FALSE
    ),
    br(),
    djpr_plot_ui("services_trade_line_chart"),
    djpr_plot_ui("NSW_Vic_Services_line_chart"),
    br(),
    h2(br(), "Balance of Trade "),
    djpr_plot_ui("trade_balance_line_chart")
  )
}
