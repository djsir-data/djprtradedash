
server <- function(input, output, session) {

  set_hcharts_options()

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


  # Service page content
  observeEvent(
    eventExpr = {
      input$service_state_comp_inp
      input$service_state_comp_states
      },
    handlerExpr = {
    update_service_state_comp(
      product = input$service_state_comp_inp,
      states = input$service_state_comp_states
      )
  })

  # merch explorer content
  output$merch_explorer <- renderHighchart({
      highcharts_merch_explorer(
        countries = req(input$merch_countries),
        goods = req(input$merch_sitc),
        origin_state = "Victoria",
        facet_by = input$merch_explorer_facets,
        smooth = input$merch_explorer_smooth
      )
    })


}
