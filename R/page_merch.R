page_merchUI <- function(...) {

  shiny::fluidPage(

    fluidRow(
      column(
        12,
        h2(
          "Victorian trade dashboard",
          style = "margin-top: 2rem; margin-bottom: 2rem;"
        )
      )
    ),
    fluidRow(
      column(
        12,
        # style = "font-size: 16px;",
        img(
          src = "containers.jpg",
          style = "border-radius: 1.25rem;float: left; margin-right: 1rem; margin-right: 1rem;",
          width = "402",
          height = "268",
          alt = "Shipping containers"
        ),
        p(
          "The Victorian Trade Dashboard helps business explore the latest ",
          "ABS trade data to gauge individual export market performance and",
          " get the latest information on Victoria’s overall trade position.",
          br(), br(),
          "On this page, you can explore specific goods export markets based",
          " on destination and type of product. Products are classified",
          " according to the standard international trade classification",
          " (SITC) to find your specific markets, search below or lookup",
          " a product category here. You can download all information as",
          " chart images or data tables via chart menus. ",
          br()
        ),

        tags$ul(
          tags$li(
            a(
              href = "#",
              class = "merchLink",
              "Find your merchandise export market's performance")
          ),
          tags$li(
            a(
              href = "#",
              class = "bopLink",
              "Explore Victoria's overall trade performance"
            )
          ),
          tags$li(
            a(
              href = "#",
              class = "servicesLink",
              "Compare Victoria's service exports"
            )
          )
        )
      )
    ),


    "Merchandise exports" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),
    shiny::fluidRow(

      # Chart option selectors
      shinydashboard::box(
        title = "Explorer options",
        width = 4,
        collapsible = TRUE,
        shinyWidgets::multiInput(
          inputId = "merch_countries",
          label = "Select export destinations",
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
        tags$label("Options"),
        shiny::fluidRow(
          style = "background-color: #FFFFFF; border-radius: 1.25rem;margin:1px;padding:10px;",
          shiny::column(
            6,
            shinyWidgets::awesomeRadio(
              width = "80%",
              inputId = "merch_explorer_sitc",
              label = "SITC Level: ",
              choices = c(
                1,
                2,
                3,
                "All"
              ),
              selected = 3,
              status = "primary"
            )
          ),
          shiny::column(
            6,
            fluidRow(
              tags$label("12 month average"),
              shinyWidgets::materialSwitch(
                "merch_explorer_smooth",
                status = "primary",
                value = TRUE,
                inline = T
              )
            ),
            shinyWidgets::awesomeRadio(
              width = "80%",
              inputId = "merch_explorer_facets",
              label = "Facet on:",
              choices = c(
                "Destination country" = "country_dest",
                "Good type" = "sitc"
              ),
              selected = "country_dest",
              status = "primary"
            ) %>% fluidRow()

          )
        )
      ) %>%
        tagAppendAttributes(
          style = "background:var(--twilight);",
          .cssSelector = ".box"
        ) %>%
        tagAppendAttributes(
          style = "padding:15px;",
          .cssSelector = ".box-body"
        ) %>%
        tagAppendAttributes(
          style = "font-size:20px;font-weight:bold;",
          .cssSelector = ".box-header h3"
        ) %>%
        to_col_xl(),
      column(
        8,
        div(
          class = "box",
          style = "padding:15px;",
          fluidRow(
            column(
              12,
              highchartOutput("merch_explorer", height = "auto")
            )
          )
        )
      )
    ),

    footer()
  )
}
