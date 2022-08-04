
ui <- function() {

  custom_header <- tags$header(
    class = "main-header fixed",
    tags$a(
      class = "sidebar-toggle",
      href = "#",
      `data-toggle` = "offcanvas",
      role = "button",
      span(
        class = "sr-only",
        "Toggle navigation"
      )
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
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "djprshiny/dashboard.css"),
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "djprshiny/bs5-card2.css"),
        includeCSS("www/djpr-theme.css")
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
