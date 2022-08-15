page_launchpadUI <- function(id) {


  shiny::fluidPage(

    shinydashboard::box(
      title = "Victorian trade data",
      width = 12,
      collapsible = T,
      p(
        "This dashboard explores the latest Australian Bureau of Statisitics",
        " (ABS) data on Victoria's trade of goods and services. "
      ),
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
    ) %>%
      tagAppendAttributes(
        style = "background:var(--twilight);background-image: url(\"containers.png\");background-repeat: no-repeat;border-radius: 1.25rem;min-height: 32px;max-height: 340px;background-size: 30% 340px;",
        .cssSelector = ".box"
        ) %>%
      tagAppendAttributes(
        style = "padding-left:33%;font-size:16px;",
        .cssSelector = ".box-body"
      ) %>%
      tagAppendAttributes(
        style = "padding-left:33%;",
        .cssSelector = ".box-header"
      ) %>%
      tagAppendAttributes(
        style = "font-size:20px;font-weight:bold;",
        .cssSelector = ".box-header h3"
      ) %>%
      to_col_xl() %>%
      fluidRow(),



    # Launchpad text & export plot
    "Victorian trade at a glance" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    shiny::fluidRow(
      column(
        6,
        shiny::div(
          class = "box",
          div(
            class="box-body",
            readRDS("inst/launchpad_services.rds")
          )
        )
      ),
      column(
        6,
        shiny::div(
          class = "box",
          div(
            class="box-body",
            readRDS("inst/launchpad_goods.rds")
          )
        )
      )

    ),


    # Latest changes
    "Latest changes in Victorian trade" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    shiny::fluidRow(
      column(
        6,
        shiny::div(
          class = "box",
          div(
            class="box-body",
            readRDS("inst/launchpad_bop.rds")
          )
        )
      ),
      column(
        6,
        shiny::div(
          class = "box",
          div(
            class="box-body",
            readRDS("inst/launchpad_rising_goods.rds")
          )
        )
      )
    ),

    # Top traders - countries
    "Top merchandise trading partners" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    shiny::fluidRow(
      shinydashboard::box(
        title = "Top export destinations",
        readRDS("inst/launchpad_exp_country_table.rds")
      )%>%
        to_col_xl() ,
      shinydashboard::box(
        title = "Top import sources",
        readRDS("inst/launchpad_imp_country_table.rds")
      )%>%
        to_col_xl()
    ),

    # Product tables
    "Top trading merchandise" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    shiny::fluidRow(
      shinydashboard::box(
        title = "Top export products",
        readRDS("inst/launchpad_product_exp_table.rds")
      )%>%
        to_col_xl() ,
      shinydashboard::box(
        title = "Top import products",
        readRDS("inst/launchpad_product_imp_table.rds")
      )%>%
        to_col_xl()
    ),

    # BOP table
    "Victoria's overall trade position" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    shinydashboard::box(
      readRDS("inst/launchpad_bop_table.rds"),
      width = 12
    ) %>%
      to_col_xl() %>%
      shiny::fluidRow(),

    footer()
  )
}

