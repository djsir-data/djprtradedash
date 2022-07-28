
ui <- function() {
  shinydashboard::dashboardPage(
    title = "Victorian International Trade",
    header = shinydashboard::dashboardHeader(
      title = shiny::HTML("Victorian<br/>International Trade"),
      titleWidth = "40%"),
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
      djprshiny::djpr_dash_theme(),
      shiny::tags$script("$('html').attr(\"lang\", \"en\")"),
      shiny::tags$script("$('section.content').attr(\"role\", \"main\")"),
      shiny::tags$style(".highchart{padding-left:10px;padding-top:10px;padding-right:15px;padding-bottom:10px;}"),
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
