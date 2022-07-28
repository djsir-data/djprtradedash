page_launchpadUI <- function(id) {


  shiny::fluidPage(

      # Launchpad text & export plot
      djprshiny::djpr_h2_box("Victorian trade at a glance") %>% shiny::fluidRow(),
      shiny::fluidRow(
        column(
          6,
          shiny::div(
            class = "box",
            readRDS("inst/launchpad_services.rds")
          )
        ),
        column(
          6,
          shiny::div(
            class = "box",
            readRDS("inst/launchpad_goods.rds")
          )
        )

      ),

      # Cards
      shiny::fluidRow(
        column(
          4,

          shiny::div(
            class = 'card text-white bg-dark mb-3',
            style = 'height:200px;',
            shiny::div(
              class = 'card-header',
              shiny::h3('Merchandise Exports Explorer')
            ),
            shiny::div(
              class = 'card-body',
              shiny::div(
                class = 'row',
                shiny::div(
                  class = 'col',
                  'Find your product and country based markets in the merchandise explorer',
                  shiny::div(
                    style = 'position:absolute;bottom:20px',
                    shinyWidgets::actionBttn(
                      'btn_explore', "Explore",
                      style = 'material-flat',
                      color = 'success'
                      )
                    )
                  ),
                shiny::div(
                  class = 'col-6',
                  style = "text-align: right;padding-bottom:15px;",
                  shiny::img(
                    style = 'display:inline;height:10vh;width:auto;',
                    src = 'https://icongr.am/material/binoculars.svg?size=164&color=fafafa',
                    alt = 'small icon of binoculars'
                  )
                )
              )
            )
          )
        ),
        column(
          4,

          shiny::div(
            class = 'card text-white bg-dark mb-3',
            style = 'height:200px;',
            shiny::div(
              class = 'card-header',
              shiny::h3("Balance of Payments")),
            shiny:: div(
              class = 'card-body',
              shiny::div(
                class = 'row',
                shiny::div(
                  class = 'col',
                  "Information on Victoria's overall trade position",
                  shiny::div(
                    style = 'position:absolute;bottom:20px',
                    shinyWidgets::actionBttn(
                      'btn_balance', "BOP",
                      style = 'material-flat',
                      color = 'success'))),
                shiny::div(
                  class = 'col-6',
                  style = "text-align: right;padding-bottom:15px;",
                  shiny::img(
                    style = 'display:inline;height:10vh;width:auto;',
                    src = 'https://icongr.am/clarity/balance.svg?size=164&color=fafafa',
                    alt = 'small icon of weighing balance')))
            )
          )
        ),
        column(
          4,

          shiny::div(
            class = 'card text-white bg-dark mb-3',
            style = 'height:200px;',
            shiny::div(
              class = 'card-header',
              shiny::h3("Service trade")),
            shiny::div(
              class = 'card-body',
              shiny::div(
                class = 'row',
                shiny::div(
                  class = 'col',
                  "Explore information on Victoria's international trade in services",
                  shiny::div(
                    style = 'position:absolute;bottom:20px',
                    shinyWidgets::actionBttn(
                      'btn_service', 'Services',
                      style = 'material-flat',
                      color = 'success'))),
                shiny::div(
                  class="col-6",
                  style = "text-align: right;padding-bottom:15px;",
                  shiny::img(
                    style = 'display:inline;height:10vh;width:auto;',
                    src = 'https://icongr.am/clarity/network-globe.svg?size=128&color=fafafa',
                    alt = 'small icon of I for')))
            )
          )
        )
        ),


      # Latest changes
      djprshiny::djpr_h2_box("Latest changes in Victorian trade") %>% shiny::fluidRow(),

      shiny::fluidRow(
        column(
          6,
          shiny::div(class = "box", readRDS("inst/launchpad_bop.rds"))
        ),
        column(
          6,
          shiny::div(class = "box", readRDS("inst/launchpad_rising_goods.rds"))
        )
      ),

      # Top traders - countries
      djprshiny::djpr_h2_box("Top merchandise trading partners") %>% shiny::fluidRow(),
      shiny::fluidRow(
        shinydashboard::box(
          title = shiny::h3("Top 5 Exports ($m)"),
          readRDS("inst/launchpad_exp_country_table.rds")
        ),
        shinydashboard::box(
          title = shiny::h3("Top 5 Imports ($m)"),
          readRDS("inst/launchpad_imp_country_table.rds")
        )
      ),

      # Product tables
      djprshiny::djpr_h2_box("Top trading merchandise") %>% shiny::fluidRow(),

      shiny::fluidRow(
        shinydashboard::box(
          title = shiny::h3("Top 5 Exports ($m)"),
          readRDS("inst/launchpad_product_exp_table.rds")
        ),
        shinydashboard::box(
          title = shiny::h3("Top 5 Imports ($m)"),
          readRDS("inst/launchpad_product_imp_table.rds")
        )
      ),

      # BOP table
      djprshiny::djpr_h2_box("Victoria's overall trade position") %>% shiny::fluidRow(),
      shinydashboard::box(
        readRDS("inst/launchpad_bop_table.rds"),
        width = 12
      ) %>% shiny::fluidRow()

  )
}






page_launchpad <- function(input, output, session, table_rowcount = 5){


  # info nav buttons
  observeEvent(input$btn_explore, {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "merch")
  })
  observeEvent(input$btn_balance, {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "bop")
  })
  observeEvent(input$btn_service, {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "services")
  })



}
