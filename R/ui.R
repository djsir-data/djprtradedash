
ui <- function() {
  shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(
      title = shiny::HTML("Victorian<br/>International Trade"),
      titleWidth = "40%"
      ),
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Home", tabName = "launchpad", selected = TRUE),
        shinydashboard::menuItem("Export explorer", tabName = "merch"),
        shinydashboard::menuItem("Balance of payments", tabName = "bop"),
        shinydashboard::menuItem("Notes", tabName = "methodology")
      ),
      width = "250px"
    ),
    body = shinydashboard::dashboardBody(
      djprshiny::djpr_dash_theme(),
      shinydashboard::tabItems(
        shinydashboard::tabItem("launchpad", page_launchpadUI('lp')),
        shinydashboard::tabItem("merch", page_merchUI()),
        shinydashboard::tabItem("bop", page_bopUI()),
        shinydashboard::tabItem("methodology", page_methodology()),
        shinydashboard::tabItem("accessibility",
                                shiny::tagList(
                                  h2(style = 'padding-top:80px',
                                     'axe.js Output'),
                                  includeScript('inst/js/axe.min.js'),
                                  shiny::tags$div(id = 'axe-results'),
                                  includeScript('inst/js/run_axe.js')
                                ))
      )
    )
  )
}
