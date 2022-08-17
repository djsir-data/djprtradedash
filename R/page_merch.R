page_merchUI <- function(...) {

  shiny::fluidPage(

    # Header banner
    div(
      class = "col-xl-12",
      # Manual box class
      div(
        class = "box",
        style = "background-color:var(--twilight);display:table;",
        # Left triangle clip image
        div(
          style = paste0(
            "width: 30%;",
            "background-image: url(\"containers.png\");",
            "background-repeat: no-repeat;",
            "background-size: ", round(1920 / 2.5), "px ", round(1280 / 2.5), "px;",
            "clip-path: polygon(0 0, 100% 0, 45% 100%, 0% 100%);",
            "display: table-cell;",
            "border-radius: 1.25rem 0 0 1.25rem;"
          )
        ),
        # Header with collapse
        div(
          class = "box-header",
          h3(
            style = "font-size:20px;font-weight:bold;",
            "Victorian trade data"
          ),
          # Header Collapse
          div(
            class="box-tools pull-right",
            tags$button(
              class = "btn btn-box-tool",
              `data-widget` = "collapse",
              tags$i(
                class = "fa fa-minus",
                role = "presentation",
                `aria-label` = "minus icon"
              )
            )
          )
        ),
        # Content
        div(
          class = "box-body",
          style = "font-size:16px;",
          # Content text
          p("This site helps exporters guage market size and performance for various goods and services."),
          # List of links
          tags$ul(
            class = "fa-ul",
            tags$li(
              span(class = "fa-li", tags$i(class = "fa fa-caret-left")),
              a(
                href = "#",
                class = "merchLink",
                "Find your merchandise export market's performance")
            ),
            tags$li(
              span(class = "fa-li", tags$i(class = "fa fa-caret-left")),
              a(
                href = "#",
                class = "bopLink",
                "Explore Victoria's overall trade performance"
              )
            ),
            tags$li(
              span(class = "fa-li", tags$i(class = "fa fa-caret-left")),
              a(
                href = "#",
                class = "servicesLink",
                "Compare Victoria's service exports"
              )
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
