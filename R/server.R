
server <- function(input, output, session) {

  plt_change <- shiny::reactive(input$plt_change) %>%
    shiny::debounce(2)

  #Launchpad searchbar
  server <- function(input, output, session) {
    output$res <- shiny::renderPrint({
      input$search
    })
  }

  # Page Footnotes

  footnote <- shiny::reactive({
    # req(dash_data)
    # latest <- max(series_latestdates)
    shiny::div(
      shiny::HTML(
        paste0(
          "This dashboard is produced by the <b>Strategy and Priority ",
          "Projects - Data + Analytics</b> team at the Victorian Department ",
          "of Jobs, Precincts and Regions. ", # The <b>latest data in this ",
          # "dashboard is for ",
          # format(latest, "%B %Y"),
          'Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Trade Dashboard">email us</a> with any comments or feedback.'
        )
      ),
      style = "color: #828282; font-size: 0.75rem"
    )
  })

  output$methodology_footnote <- output$launchpad_footnote <- output$bop_footnote <- output$merch_footnote <- output$country_footnote <- shiny::renderUI({
    footnote()
  })

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
