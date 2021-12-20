page_bop <- function(...) {
  djpr_tab_panel(
    title = "Balance of Payments",
    h1("Key indicators"),
    paste0("This page contains Victoria's international transactions, typically quarterly or
    yearly, over a particular period.  It shows the sum of the transactions of those involving
    goods or services."),
    h2(br(), "Goods"),
    h2(br(), "Services"),
    h2(br(), "Balance of Trade "),
  )
}
