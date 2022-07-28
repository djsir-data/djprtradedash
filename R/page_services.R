page_servicesUI <- function(...){
  shiny::fluidPage(

    # Top pie chart
    djprshiny::djpr_h2_box("What services are victoria trading?") %>% fluidRow(),
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
    )


    #
  )
}
