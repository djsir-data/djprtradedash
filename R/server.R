#' @import djprshiny
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import djprtheme
#' @importFrom rlang .data .env

server <- function(input, output, session) {

  plt_change <- reactive(input$plt_change) %>%
    debounce(2)

  #Launchpad searchbar
  server <- function(input, output, session) {
    output$res <- renderPrint({
      input$search
    })
  }

  # Page Footnotes

  footnote <- reactive({
    # req(dash_data)
    # latest <- max(series_latestdates)
    div(
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

  output$methodology_footnote <- output$launchpad_footnote <- output$bop_footnote <- output$merch_footnote <- output$country_footnote <- renderUI({
    footnote()
  })

  print('up2 module')


  page_launchpad(input, output, session, plt_change, table_rowcount = 5)

  page_bop(input, output, session, plt_change, table_rowcount = 5)





  # Initialise country selection & URL query
  updateSelectInput(
    inputId = "country_select",
    choices = merch_country_dest,
    selected = "China (excludes SARs and Taiwan)"
  )

  output$country_select <- renderText(
    paste0(
      "Victoria-",
      stringr::str_remove_all(input$country_select, " (.+)"),
      " Trade"
    )
  )

  merch_df <- shiny::reactive({
    if(input$merch_explorer_sitc %in% c(1,2,3)) {
      merch <- merch %>%
        dplyr::filter(nchar(.data$sitc_code) == input$merch_explorer_sitc) %>%
        dplyr::collect()
    } else {}
    merch |>
      dplyr::mutate(code_name = paste0(.data$sitc_code, ": ", .data$sitc)) |>
      collect()
  })

  observeEvent(merch_df(), {
    shinyWidgets::updateMultiInput(session = session, inputId = "merch_sitc",
                                   choices = unique(merch_df()$code_name))
  })

  merch_explorer_plot <- shiny::reactive({
    shiny::req(
      input$merch_explorer_dates,
      input$merch_countries,
      input$merch_sitc,
      input$merch_explorer_facets,
      input$merch_explorer_sitc
    )

    merch %>%
      dplyr::filter(
        .data$date >= !!input$merch_explorer_dates[1],
        .data$date <= !!input$merch_explorer_dates[2]
      ) %>%
      viz_merch_explorer(
        countries = input$merch_countries,
        goods = sub(".[0-9]*:\\s", "", input$merch_sitc),
        facet_by = input$merch_explorer_facets,
        smooth = input$merch_explorer_smooth
      )
  })

  output$merch_explorer <- shiny::renderPlot({
    merch_explorer_plot()
  })

  djprshiny::download_server(
    id = "merch_explorer_dl",
    plot = merch_explorer_plot(),
    plot_name = "merch_explorer_plot"
  )



  # Country profiles
  # djpr_plot_server(
  #   "country_top_exp",
  #   viz_country_top_exp,
  #   data = merch,
  #   data_imp = merch_imp,
  #   plt_change = plt_change,
  #   country_select = reactive(input$country_select),
  #   date_slider_value_min = Sys.Date() - lubridate::years(3),
  #   height_percent = 160
  # )
  #
  # output$country_1y_exp_stat <- reactive({
  #   viz_country_1y_exp_stat(merch, input$country_select)
  # })
  #
  # output$country_1y_imp_stat <- reactive({
  #   viz_country_1y_imp_stat(merch_imp, input$country_select)
  # })
  #
  # output$country_1y_exp_change_stat <- reactive({
  #   viz_country_1y_exp_change_stat(merch, input$country_select)
  # })


  # Notes
  # SITC Information and Explorer

  sitc_merch <- merch %>%
      filter(country_dest == "Total",
             date >= (max(merch_dates$max) - months(12))) %>%
      group_by(sitc_code, sitc) %>%
      summarise(sum_value = sum(value)) %>%
      mutate(sitc_level = as.character(nchar(sitc_code))) %>%
      rename(`SITC Level` = sitc_level,
             `SITC Code` = sitc_code,
             `SITC Name` = sitc,
             `Total Exports in Last 12 Months ($000s)` = sum_value)

  output$sitc_table <- DT::renderDT(
    sitc_merch,
    filter = "top"
  )

}
