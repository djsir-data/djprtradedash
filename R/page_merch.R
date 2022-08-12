page_merchUI <- function(...) {

  shiny::fluidPage(

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
            ),
          )
        )
      )
    )
  )
}
