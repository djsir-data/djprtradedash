page_servicesUI <- function(...){
  shiny::fluidPage(

    # Top pie chart
    "What services is Victoria trading?" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    shiny::fluidRow(
        shinydashboard::box(
          width = 8,
          readRDS("inst/services_composition.rds")
        ) %>%
          to_col_xl(),

        shinydashboard::box(
          title = "ABS service trade classification",
          width = 4,
          collapsible = TRUE,
          readRDS("inst/service_category_list.rds")
        ) %>%
          tagAppendAttributes(
            style = "background:var(--twilight);max-height:670px;overflow-y:auto;overflow-x:crop;",
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
          to_col_xl()
    ),


    "Comparing service exports to other states" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      shinydashboard::box(
        title = "Chart options",
        width = 3,
        collapsible = TRUE,
        selectInput(
          "service_state_comp_inp",
          "Select service export",
          choices = service_categories,
          selected = "Education travel",
          multiple = FALSE
        ),
        shinyWidgets::radioGroupButtons(
          "service_state_comp_period",
          label = "Reference period",
          choices = c("Calendar year" = "Calendar Year", "Financial year" = "Financial Year"),
          selected = "Calendar Year",
          justified = TRUE,
          status = "secondary"
        ),
        checkboxGroupInput(
          "service_state_comp_states",
          "Select states to display",
          choices = c(
            "Vic.",
            "NSW",
            "Qld",
            "WA",
            "ACT",
            "NT",
            "SA",
            "Tas."
          ),
          selected = c(
            "Vic.",
            "NSW",
            "Qld",
            "WA",
            "ACT",
            "NT",
            "SA",
            "Tas."
          )
        )
      ) %>%
        tagAppendAttributes(
          style = "background:var(--twilight);overflow-y:auto;overflow-x:crop;",
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
        9,
        div(
          class = "box",
          readRDS("inst/service_state_comparison.rds")
        )
      )
    ),

    footer()


  )



}
