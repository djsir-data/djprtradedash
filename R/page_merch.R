page_merch <- function(...) {
  # djpr_tab_panel(
  selecter_height <- 250
  inner_height <- selecter_height - 44

  tabPanel(
    title = "Merchandise exports",
    tags$head(
      tags$style(paste0(".multi-wrapper {height: ", selecter_height, "px;}")),
      tags$style(paste0(".multi-wrapper .non-selected-wrapper, .multi-wrapper .selected-wrapper {height: ",
                 inner_height,
                 "px;}"))
    ),
    ggiraph_js(),
    value = "tab-merchandise-exports",
    # toc_space = 2,
    # right_space = 1,
    br(),
    br(),
    h1("Merchandise Exports Data Explorer"),
    fluidRow(
      column(4,
             shinyWidgets::multiInput(
               inputId = "merch_countries",
                   label = "Export destinations: ",
                   choices = sort(unique(as.character(merch$country_dest))),
                   selected = "Thailand",
                   width = "100%",
                   options = list(
                     non_selected_header = "All destinations:",
                     selected_header = "Selected destinations:"
                   )

                 ),
             shinyWidgets::multiInput(
               inputId = "merch_sitc",
               label = "Goods",
               choices = sort(unique(as.character(merch$sitc_rev3))),
               selected = "Medicinal and pharmaceutical products",
               width = "100%",
               options = list(
                 non_selected_header = "All goods:",
                 selected_header = "Selected goods:"
               )
             )
      ),
      column(8,
             # djpr_plot_ui("merch_explorer")
             shinyWidgets::awesomeRadio(
               inputId = "merch_explorer_facets",
               label = "Facet on: ",
               choices = c("Destination country" = "country_dest",
                           "Good type" = "sitc_rev3"),
               selected = "country_dest",
               inline = TRUE,
               status = "primary"
             ),
             plotOutput("merch_explorer",
                        height = "600px"),
             fluidRow(
               column(8,
                      sliderInput("merch_explorer_dates",
                                  label = "",
                                  min = min(merch$date),
                                  max = max(merch$date),
                                  value = c(
                                    min(merch$date),
                                    max(merch$date)
                                  ),
                                  dragRange = TRUE,
                                  timeFormat = "%b %Y",
                                  ticks = FALSE)
               ),
               column(4,
                      br(),
                      djprshiny::download_ui("merch_explorer_dl"))
             )
             )


  )
  )
}
