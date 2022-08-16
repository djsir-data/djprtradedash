page_launchpadUI <- function(id) {


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
          p(
            "This dashboard explores the latest Australian Bureau of Statisitics",
            " (ABS) data on Victoria's trade of goods and services. Click menu ",
            "icons for chart and data downloads."
          ),
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

