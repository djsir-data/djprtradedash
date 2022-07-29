page_merchUI <- function(...) {

  shiny::fluidPage(

    djprshiny::djpr_h2_box("Merchandise exports") %>% fluidRow(),
    shiny::fluidRow(

      # Chart option selectors
      column(
        4,
        div(
          class = "box",
          style = "padding:15px;",
          shiny::column(
            12,
            h3("Explorer options") %>% fluidRow(),
            br(),
            shinyWidgets::multiInput(
              inputId = "merch_countries",
              label = "Select Export destinations: ",
              choices = sort(merch_country_dest),
              selected = c("Thailand", "Malaysia"),
              width = "100%",
              options = list(
                non_selected_header = "All destinations:",
                selected_header = "Selected destinations:"
              )
            ) %>% fluidRow(),
            br(),
            shinyWidgets::multiInput(
              inputId = "merch_sitc",
              label = "Select Goods",
              choices = sort(merch_sitc_lu$sitc),
              selected = "Medicinal and pharmaceutical products (excl. medicaments of group 542)",
              width = "100%",
              options = list(
                non_selected_header = "All goods:",
                selected_header = "Selected goods:"
              )
            )%>% fluidRow(),
            br(),
            shiny::fluidRow(
              shiny::column(
                6,
                shinyWidgets::awesomeRadio(
                  width = "80%",
                  inputId = "merch_explorer_sitc",
                  label = "Select SITC Level: ",
                  choices = c(
                    1,
                    2,
                    3,
                    "All"
                  ),
                  selected = 3,
                  # inline = TRUE,
                  status = "primary"
                )
              ),
              shiny::column(
                6,
                shinyWidgets::awesomeRadio(
                  width = "80%",
                  inputId = "merch_explorer_facets",
                  label = "Select Facet on: ",
                  choices = c(
                    "Destination country" = "country_dest",
                    "Good type" = "sitc"
                  ),
                  selected = "country_dest",
                  # inline = TRUE,
                  status = "primary"
                ) %>% fluidRow(),
                shinyWidgets::materialSwitch(
                  "merch_explorer_smooth",
                  label = "Smooth with 12 month average",
                  status = "primary",
                  value = TRUE
                ) %>% fluidRow()
              )
          )

        )
      )
      ),
      column(
        8,
        div(
          class = "box",
          style = "padding:15px;",
          fluidRow(
            column(
              12,
              plotOutput("merch_explorer", height = "600px")
            ),
          ),
          br(),
          shiny::fluidRow(
            shiny::column(
              10,
              shiny::sliderInput(
                width = "80%",
                "merch_explorer_dates",
                label = "Select Dates",
                min = merch_dates$min,
                max = merch_dates$max,
                value = c(
                  merch_dates$min,
                  merch_dates$max
                ),
                dragRange = TRUE,
                timeFormat = "%b %Y",
                ticks = FALSE
              )
            ),
            shiny::column(
              2,
              shiny::br(),
              djprshiny::download_ui("merch_explorer_dl")
            )
          )
        )
      )
    )
  )
}


page_merch <- function(input, output, session, plt_change){

  sitc_lu <- shiny::reactive({
    if(input$merch_explorer_sitc %in% c(1,2,3)) {
      lu <- merch_sitc_lu %>%
        dplyr::filter(n == !!input$merch_explorer_sitc)
    } else {
      lu <- merch_sitc_lu %>%
        dplyr::mutate(sitc = paste0(.data$sitc_code, ": ", .data$sitc))
    }
    lu %>%
      dplyr::pull(.data$sitc) %>%
      unique()
  })

  shiny::observe({
    shinyWidgets::updateMultiInput(session = session, inputId = "merch_sitc",
                                   choices = sitc_lu())
  })

  shiny::observe({

    shiny::req(
      input$merch_explorer_dates,
      input$merch_countries,
      input$merch_sitc,
      input$merch_explorer_facets,
      input$merch_explorer_sitc
    )

    mindate <- input$merch_explorer_dates[1]
    maxdate <- input$merch_explorer_dates[2]

    merch_plt_data <- merch %>%
      dplyr::filter(
        .data$date >= !!mindate,
        .data$date <= !!maxdate
      )

    merch_plt <- viz_merch_explorer(
      merch_plt_data,
      countries = input$merch_countries,
      goods = sub(".[0-9]*:\\s", "", input$merch_sitc),
      facet_by = input$merch_explorer_facets,
      smooth = input$merch_explorer_smooth,
      merch_explorer_sitc = input$merch_explorer_sitc
    )

    output$merch_explorer <- renderPlot({
      merch_plt
    })

  })




  djprshiny::download_server(
    id = "merch_explorer_dl",
    plot = merch_explorer_plot(),
    plot_name = "merch_explorer_plot"
  )
}
