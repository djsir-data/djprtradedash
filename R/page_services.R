page_servicesUI <- function(...){
  shiny::fluidPage(

    # Top pie chart
    djprshiny::djpr_h2_box("What services is victoria trading?") %>% fluidRow(),
    shiny::fluidRow(
        column(
          8,
          shiny::div(
            class = "box",
            style = "padding:10px;", ##CHECK
            readRDS("inst/services_composition.rds")
          )
        ),
        column(
          4,
          shiny::div(
            class = "box",
            style = "padding-left:20px;padding-top:10px;padding-right:10px;padding-bottom:10px;height:670px;overflow-y:auto;overflow-x:clip;",
            h3("ABS service trade classification"),
            readRDS("inst/service_category_list.rds")
          )
        )
    ),


    djprshiny::djpr_h2_box("Comparing service exports to other states") %>% fluidRow(),
    fluidRow(
      column(
        3,
        div(
          class = "box",
          style = "padding:20px;height:500px;overflow-y:visible;",
          h3("Chart options"),
          br(),
          selectInput(
            "service_state_comp_inp",
            "Select service export",
            choices = service_categories,
            selected = "Education travel",
            multiple = FALSE
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
        )
      ),
      column(
        9,
        div(
          class = "box",
          readRDS("inst/service_state_comparison.rds")
        )
      )
    )


    #
  )
}
