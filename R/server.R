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
          'Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard">email us</a> with any comments or feedback.'
        )
      ),
      style = "color: #828282; font-size: 0.75rem"
    )
  })

  output$launchpad_footnote <- output$bop_footnote <- output$merch_footnote <- output$country_footnote <- renderUI({
    footnote()
  })

  print('up2 module')

  page_launchpad(input, output, session, plt_change, table_rowcount = 5)

  page_bop(input, output, session, plt_change, table_rowcount = 5)



  # Initialise country selection & URL query
  updateSelectInput(
    inputId = "country_select",
    choices = unique(merch$country_dest),
    selected = "China (excludes SARs and Taiwan)"
  )

  output$country_select <- renderText(
    paste0(
      "Victoria-",
      stringr::str_remove_all(input$country_select, " (.+)"),
      " Trade"
    )
  )

  merch_explorer_plot <- shiny::reactive({
    shiny::req(
      input$merch_explorer_dates,
      input$merch_countries,
      input$merch_sitc,
      input$merch_explorer_facets
    )

    merch %>%
      dplyr::filter(
        .data$date >= !!input$merch_explorer_dates[1],
        .data$date <= !!input$merch_explorer_dates[2]
      ) %>%
      viz_merch_explorer(
        countries = input$merch_countries,
        goods = input$merch_sitc,
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

}
