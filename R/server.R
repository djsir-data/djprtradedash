
server <- function(input, output, session) {

  page_launchpad(input, output, session, table_rowcount = 5)
  page_bop(input, output, session, plt_change, table_rowcount = 5)
  page_merch(input, output, session, plt_change)


  merch_last_12 <- merch_dates$max - months(12)

  sitc_merch <- merch %>%
    dplyr::filter(country_dest == "Total", date >= !!merch_last_12) %>%
    dplyr::group_by(sitc_code, sitc) %>%
    dplyr::summarise(sum_value = sum(value)) %>%
    dplyr::collect() %>%
    dplyr::mutate(sitc_level = as.character(nchar(sitc_code))) %>%
    dplyr::rename(
      `SITC Level` = sitc_level,
      `SITC Code` = sitc_code,
      `SITC Name` = sitc,
      `Total Exports in Last 12 Months ($000s)` = sum_value
      )

  output$sitc_table <- DT::renderDT(
    sitc_merch,
    filter = "top"
  )

}
