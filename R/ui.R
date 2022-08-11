
ui <- function() {

  custom_header <- shinydashboard::dashboardHeader()
  custom_header$children[[2]] <- NULL
  custom_header <- custom_header %>%
    tagAppendChildren(
      .cssSelector = ".navbar-custom-menu",
      fluidRow(
        div(class = "col", span(HTML("Latest</br>ABS data"), class = "badgeDescript")),
        div(class = "col", badge_bop()),
        div(class = "col", badge_goods()),
        div(class = "col", badge_services())
      )
    )


  shinydashboard::dashboardPage(
    title = "Victorian International Trade",
    header = custom_header,
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(id = 'tabs',
        shinydashboard::menuItem("Home", tabName = "launchpad", selected = TRUE),
        shinydashboard::menuItem("Export explorer", tabName = "merch"),
        shinydashboard::menuItem("Balance of payments", tabName = "bop"),
        shinydashboard::menuItem("Services", tabName = "services"),
        shinydashboard::menuItem("Notes", tabName = "methodology")
      ),
      width = "250px"
    ),
    body = shinydashboard::dashboardBody(
      tags$head(
        # includeCSS("www/tradedash.css")
        # shiny::tags$link(rel = "stylesheet", type = "text/css", href = "djprshiny/dashboard.css"),
        # shiny::tags$link(rel = "stylesheet", type = "text/css", href = "djprshiny/bs5-card2.css"),
        tags$link(
          href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
          rel = "stylesheet",
          integrity = "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
          crossorigin = "anonymous"
        ),
        includeCSS("www/globalvic.css")
        ),
      shiny::tags$script("$('html').attr(\"lang\", \"en\")"),
      shiny::tags$script("$('section.content').attr(\"role\", \"main\")"),
      shinydashboard::tabItems(
        shinydashboard::tabItem("launchpad", page_launchpadUI('lp')),
        shinydashboard::tabItem("merch", page_merchUI()),
        shinydashboard::tabItem("bop", page_bopUI()),
        shinydashboard::tabItem("services", page_servicesUI()),
        shinydashboard::tabItem("methodology", page_methodology())
      )
    )
  )
}
