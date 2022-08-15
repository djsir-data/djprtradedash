page_methodology <- function(...) {
  shiny::fluidPage(

    "Trade dashboard sources and notes" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      column(
        12,
        shiny::includeMarkdown("R/methodology.md")
      )
    ),

    "SITC Information and Explorer" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(

      shinydashboard::box(
        width = 12,
        DT::dataTableOutput("sitc_table"),
        footer = paste0(
          "Source: ABS.Stat Merchandise Exports data per commodity. Latest data is from ",
          format(merch_dates$max, "%B %Y"), "."
        )
      ) %>%
        tagAppendAttributes(
          style = "padding:15px;",
          .cssSelector = ".box-body"
        ) %>%
        tagAppendAttributes(
          style = "background:var(--twilight);padding:15px;",
          .cssSelector = ".box-footer"
        ) %>%
        tagAppendAttributes(
          style = "background:var(--twilight);",
          .cssSelector = ".box"
        ) %>%
        to_col_xl()
    ),

    "Glossary" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      column(
        12,
        shiny::includeMarkdown("R/glossary.md")
      )
    ),

    footer()
  )

}
