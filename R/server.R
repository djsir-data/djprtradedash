
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
      input$service_state_comp_period
      },
    handlerExpr = {
    update_service_state_comp(
      product = input$service_state_comp_inp,
      states = input$service_state_comp_states,
      period = input$service_state_comp_period
      )
  })

  # merch explorer content
  output$merch_explorer <- renderHighchart({
      highcharts_merch_explorer(
        countries = req(input$merch_countries),
        goods = req(input$merch_sitc),
        origin_state = req(input$merch_origin),
        facet_by = input$merch_explorer_facets,
        smooth = input$merch_explorer_smooth
      )
    })

  # Merch explorer SITC options updating
  # Chooses parent SITC or first child SITC
  observeEvent(
    input$merch_explorer_sitc,
    {
      # Target SITC level
      new_level <- req(input$merch_explorer_sitc)


      # Data frame of current inputs
      current_vals <- merch_sitc_lu[
        match(input$merch_sitc, merch_sitc_lu$sitc),
        ]

      # All SITC and no selections
      if(new_level == "All" & nrow(current_vals) == 0){
        new_options <- merch_sitc_lu
        new_selection <- character(0)
      # ALL SITC with selections
      } else if (new_level == "All"){
        new_options <- merch_sitc_lu
        new_selection <- sort(current_vals$sitc)
      # Specific SITC with no selections
      } else if (nrow(current_vals) == 0){
        new_level <- as.integer(new_level)
        new_options <- merch_sitc_lu[merch_sitc_lu$n == new_level, ]
        new_selection <- character(0)
      # Specific SITC with selections
      } else {
        new_level <- as.integer(new_level)
        new_options <- merch_sitc_lu[merch_sitc_lu$n == new_level, ]
        new_selection <- ifelse(
          current_vals$n > new_level,
          sapply(current_vals$sitc_code, function(x) pull(new_options[new_options$sitc_code == substr(x, 1, new_level), "sitc_code"])[1]),
          sapply(current_vals$sitc_code, function(x) pull(new_options[substr(new_options$sitc_code, 1, nchar(x)) == x, "sitc_code"])[1])
          )
        new_selection <- pull(new_options[new_options$sitc_code %in% new_selection, "sitc"])
      }

      shinyWidgets::updateMultiInput(
        session = session,
        inputId = "merch_sitc",
        selected = new_selection,
        choices = sort(new_options$sitc)
      )

    }
  )




}
