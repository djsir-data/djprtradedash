page_merchUI <- function(...) {
  # djpr_tab_panel(
  selecter_height <- 325
  inner_height <- selecter_height - 44

  tabPanel(
    title = "Merchandise exports",
    tags$head(
      tags$style(paste0(".multi-wrapper {height: ", selecter_height, "px;}")),
      tags$style(paste0(
        ".multi-wrapper .non-selected-wrapper, .multi-wrapper .selected-wrapper {height: ",
        inner_height,
        "px;}"
      ))
    ),
    # ggiraph_js(),
    value = "tab-merchandise-exports",
    # toc_space = 2,
    # right_space = 1,
    br(),
    br(),
    h1("Merchandise Exports Data Explorer"),
    fluidRow(
      column(
        4,
        shinyWidgets::multiInput(
          inputId = "merch_countries",
          label = "Export destinations: ",
          choices = sort(merch_country_dest),
          selected = c("Thailand", "Malaysia"),
          width = "100%",
          options = list(
            non_selected_header = "All destinations:",
            selected_header = "Selected destinations:"
          )
        ),
        shinyWidgets::multiInput(
          inputId = "merch_sitc",
          label = "Goods",
          choices = sort(merch_sitc),
          selected = "Medicinal and pharmaceutical products (excl. medicaments of group 542)",
          width = "100%",
          options = list(
            non_selected_header = "All goods:",
            selected_header = "Selected goods:"
          )
        )
      ),
      column(
        8,
        # djpr_plot_ui("merch_explorer")
        fluidRow(
          column(
            4,
            shinyWidgets::awesomeRadio(
              inputId = "merch_explorer_sitc",
              label = "SITC Level: ",
              choices = c(
                1,
                2,
                3,
                "All"
              ),
              selected = 3,
              inline = TRUE,
              status = "primary"
            )
          ),
          column(
            4,
            shinyWidgets::awesomeRadio(
              inputId = "merch_explorer_facets",
              label = "Facet on: ",
              choices = c(
                "Destination country" = "country_dest",
                "Good type" = "sitc"
              ),
              selected = "country_dest",
              inline = TRUE,
              status = "primary"
            )
          ),
          column(
            4,
            "Smooth using:\n",
            shinyWidgets::materialSwitch("merch_explorer_smooth",
              label = "12 month rolling average",
              status = "primary",
              value = TRUE
            )
          )
        ),
        plotOutput("merch_explorer",
          height = "600px"
        ),
        fluidRow(
          column(
            8,
            sliderInput("merch_explorer_dates",
              label = "",
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
          column(
            4,
            br(),
            djprshiny::download_ui("merch_explorer_dl")
          )
        )
      )
    ),
    br(),
    centred_row(htmlOutput("merch_footnote")),
    br()
  )
}


page_merch <- function(input, output, session, plt_change, merch = merch){

  merch_df <- shiny::reactive({
    if(input$merch_explorer_sitc %in% c(1,2,3)) {
      merch <- merch %>%
        dplyr::filter(nchar(.data$sitc_code) == !!input$merch_explorer_sitc) %>%
        dplyr::collect()
    } else {
      merch <- merch %>%
      dplyr::mutate(code_name = paste0(.data$sitc_code, ": ", .data$sitc)) %>%
      dplyr::collect()
    }
    merch |> pull(code_name) |> unique()
  })

  observe({
    shinyWidgets::updateMultiInput(session = session, inputId = "merch_sitc",
                                   choices = merch_df())
  })

  observe({

    shiny::req(
      input$merch_explorer_dates,
      input$merch_countries,
      input$merch_sitc,
      input$merch_explorer_facets,
      input$merch_explorer_sitc
    )

    print('merch plot reactive')

    merch_plt <- merch %>%
      dplyr::filter(
        .data$date >= !!input$merch_explorer_dates[1],
        .data$date <= !!input$merch_explorer_dates[2]
      ) %>% #collect() %>%
      viz_merch_explorer(
        countries = input$merch_countries,
        goods = sub(".[0-9]*:\\s", "", input$merch_sitc),
        facet_by = input$merch_explorer_facets,
        smooth = input$merch_explorer_smooth,
        merch_explorer_sitc = input$merch_explorer_sitc
      )

    output$merch_explorer <- shiny::renderPlot({
      print('render merch plot')
      merch_plt
    })

  })




  djprshiny::download_server(
    id = "merch_explorer_dl",
    plot = merch_explorer_plot(),
    plot_name = "merch_explorer_plot"
  )
}
