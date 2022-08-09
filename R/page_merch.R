page_merchUI <- function(...) {

  shiny::fluidPage(

    "Merchandise exports" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>% 
      fluidRow(),
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
              highchartOutput("merch_explorer", height = "auto")
            ),
          )
        )
      )
    )
  )
}
