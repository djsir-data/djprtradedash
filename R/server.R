#' @import djprshiny
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import djprtheme
#' @importFrom rlang .data .env

server <- function(input, output, session) {
  plt_change <- reactive(input$plt_change) %>%
    debounce(2)

  # djpr_plot_server(
  #   "merch_explorer",
  #   viz_merch_explorer,
  #   data = merch,
  #   countries = reactive(input$merch_countries),
  #   goods = reactive(input$merch_sitc),
  #   plt_change = plt_change
  # )

  merch_explorer_plot <- reactive({
    req(
      input$merch_explorer_dates,
      input$merch_countries,
      input$merch_sitc,
      input$merch_explorer_facets
    )

    merch %>%
      dplyr::filter(
        .data$date >= input$merch_explorer_dates[1],
        .data$date <= input$merch_explorer_dates[2]
      ) %>%
      viz_merch_explorer(
        countries = input$merch_countries,
        goods = input$merch_sitc,
        facet_by = input$merch_explorer_facets,
        smooth = input$merch_explorer_smooth
      )
  })

  output$merch_explorer <- renderPlot({
    merch_explorer_plot()
  })

  djprshiny::download_server(
    id = "merch_explorer_dl",
    plot = merch_explorer_plot(),
    plot_name = "merch_explorer_plot"
  )
}
