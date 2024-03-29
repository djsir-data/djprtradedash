page_disclaimerUI <- function(...){
  fluidPage(
    "Disclaimer" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),

    fluidRow(
      column(
        12,
        p(
          "The content of this Victorian Government website is provided",
          " for information purposes only. No claim is made as to the accuracy",
          "or currency of any of the content on this website at any time. The ",
          "Victorian Government and this agency (the Department of Jobs, ",
          "Skills, Industry and Regions) do not accept any liability to any person",
          "for the information (or the use of such information) which is",
          "provided on this website, including information sourced or derived",
          "from third parties or incorporated into the website by reference."
          )
      )
    )
  )
}
