page_methodology <- function(...) {
  shiny::fluidPage(

    "FAQ" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    shinydashboard::box(
      title = "What does the Victorian Trade Dashboard show?",
      p(
        "The  Victorian Trade Dashboard shows detailed export and import data for Victorian",
        " goods and services. It shows goods (i.e.  physical products and",
        " commodities) which are produced in Victoria and sold to",
        " international markets (exports), and goods which are produced",
        " abroad and sold in Victoria (imports).",
        br(), br(),
        "Services exports are any service provided in Victoria to residents",
        "of another country, for example education provided at a Victorian",
        "university to an international student, or design services provided",
        "by a Victorian architecture firm to a client in another country.",
        "Services imports are services provided abroad to Victorians, such",
        "as a Victorian spending tourism dollars in another country while on",
        "holiday."
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header",
        `data-widget` = "collapse",
        cursor = "pointer",
        `aria-expanded` = "false",
        role = "button"
        ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header h3",
        class = "fa-plus",
        style = "font-weight: bold;"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-body",
        class = "collapse"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box",
        class = "collapsed-box"
      ) %>%
      to_col_xl() %>%
      fluidRow(),

    shinydashboard::box(
      title = "Where does the data come from?",
      p(
        "Data is compiled and released by the Australian Bureau of Statistics",
        " (ABS), the Department of Foreign Affairs and Trade (DFAT), and the",
        " Australian Border Force."
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header",
        `data-widget` = "collapse",
        cursor = "pointer",
        `aria-expanded` = "false",
        role = "button"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header h3",
        class = "fa-plus",
        style = "font-weight: bold;"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-body",
        class = "collapse"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box",
        class = "collapsed-box"
      ) %>%
      to_col_xl() %>%
      fluidRow(),

    shinydashboard::box(
      title = "How often is it updated?",
      p(
        "Different data sets are updated and released at different time periods throughout the year, and some are updated more frequently than others.
Services data is usually released twice annually – once by calendar year and once by financial year.
References and citations beneath respective charts will specify the time period of that data (i.e. whether it is produced on a monthly, quarterly, or annual basis).
"
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header",
        `data-widget` = "collapse",
        cursor = "pointer",
        `aria-expanded` = "false",
        role = "button"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header h3",
        class = "fa-plus",
        style = "font-weight: bold;"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-body",
        class = "collapse"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box",
        class = "collapsed-box"
      ) %>%
      to_col_xl() %>%
      fluidRow(),

    shinydashboard::box(
      title = "Can I download data or charts?",
      p(
        "All diagrams and charts have a context menu in the upper right-hand corner which allows users to: print the chart; download the chart as an image or PDF; download the underlying data as a CSV or XLS file; or view the data in a table."
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header",
        `data-widget` = "collapse",
        cursor = "pointer",
        `aria-expanded` = "false",
        role = "button"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header h3",
        class = "fa-plus",
        style = "font-weight: bold;"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-body",
        class = "collapse"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box",
        class = "collapsed-box"
      ) %>%
      to_col_xl() %>%
      fluidRow(),

    shinydashboard::box(
      title = "How does it account for goods produced in other states?",
      p(
        "State level trade data reflects the exports and imports of businesses located within a particular state. Goods that are produced in another state and shipped from a Victorian port do not count as Victorian exports and will not be reflected in the Dashboard’s charts and figures. "
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header",
        `data-widget` = "collapse",
        cursor = "pointer",
        `aria-expanded` = "false",
        role = "button"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header h3",
        class = "fa-plus",
        style = "font-weight: bold;"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-body",
        class = "collapse"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box",
        class = "collapsed-box"
      ) %>%
      to_col_xl() %>%
      fluidRow(),

    shinydashboard::box(
      title = "Why is there no services data at the market level?",
      p(
        "State-level services data is only released as total values and does not include market-specific values. Market-specific data is only available at the national (Australian) level."
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header",
        `data-widget` = "collapse",
        cursor = "pointer",
        `aria-expanded` = "false",
        role = "button"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header h3",
        class = "fa-plus",
        style = "font-weight: bold;"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-body",
        class = "collapse"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box",
        class = "collapsed-box"
      ) %>%
      to_col_xl() %>%
      fluidRow(),

    shinydashboard::box(
      title = "What are confidential items of trade?",
      p(
        "The ABS makes some data confidential for a range of reasons. When data is deemed confidential, a restriction is placed on it to suppress the level of detail available.  Depending on the nature of the restriction, this may be all detail relating to the product, or a suppression of certain elements (e.g. country/export market details)."
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header",
        `data-widget` = "collapse",
        cursor = "pointer",
        `aria-expanded` = "false",
        role = "button"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header h3",
        class = "fa-plus",
        style = "font-weight: bold;"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-body",
        class = "collapse"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box",
        class = "collapsed-box"
      ) %>%
      to_col_xl() %>%
      fluidRow(),

    shinydashboard::box(
      title = "What do terms like Balance of Payments, chain volume measures and seasonally adjusted mean?",
      p(
        "Please refer to the Glossary below"
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header",
        `data-widget` = "collapse",
        cursor = "pointer",
        `aria-expanded` = "false",
        role = "button"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-header h3",
        class = "fa-plus",
        style = "font-weight: bold;"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box-body",
        class = "collapse"
      ) %>%
      tagAppendAttributes(
        .cssSelector = ".box",
        class = "collapsed-box"
      ) %>%
      to_col_xl() %>%
      fluidRow(),

    "SITC Information" %>%
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
