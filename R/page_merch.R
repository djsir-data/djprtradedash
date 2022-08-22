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
          style = "border-radius: 1.25rem;float: left; margin-right: 1rem; margin-bottom: 1rem;",
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
          " (SITC) – to find your specific markets, search below or ",
          tags$a(
            "lookup a product category here",
            href = "#",
            class = "methodologyLink"
          ),
          ". You can download all information as chart images or data tables",
          " via chart menus. ",
          br(), br(),
          "For information on services, you can view ",
          a(
            "Victorias service export breakdown here",
            href = "#",
            class = "servicesLink"
          ),
          ". For more information on victoria's overall trade, you you can explore the",
          a(
            "trade overview here",
            href = "#",
            class = "launchpadLink"
          ),
          " or ",
          a(
            "balance of payments information here.",
            href = "#",
            class = "bopLink"
          )

        )
      )
    ),


    "Merchandise exports" %>%
      h2() %>%
      column(12, .) %>%
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
          choices = sort(
            merch_sitc_lu$sitc[
              merch_sitc_lu$n == 3
            ]
          ),
          selected = "Wheat (incl. spelt) and meslin, unmilled",
          width = "100%",
          options = list(
            non_selected_header = "All goods:",
            selected_header = "Selected goods:"
          )
        )%>% fluidRow(),
        br(),
        shiny::fluidRow(
          style = "background-color: #FFFFFF; border-radius: 1.25rem;margin:1px;padding:10px;",
          shiny::column(
            12,
            shinyWidgets::radioGroupButtons(
              inputId = "merch_explorer_sitc",
              label = "SITC Level",
              choices = c(
                1,
                2,
                3,
                "All"
              ),
              selected = 3,
              status = "primary",
              justified = T
            ),
            shinyWidgets::radioGroupButtons(
              "merch_explorer_smooth",
              status = "primary",
              label = "Data smoothing",
              choiceNames = c("No smoothing", "12 month average"),
              choiceValues = c(F,T),
              selected = TRUE,
              justified = T
            ),
            shinyWidgets::radioGroupButtons(
              inputId = "merch_explorer_facets",
              label = "Chart facet",
              choices = c(
                "Destination country" = "country_dest",
                "Good type" = "sitc"
              ),
              selected = "country_dest",
              status = "primary",
              justified = T
            )
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
