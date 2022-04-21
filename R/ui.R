
ui <- function() {
  shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(
      title = shiny::HTML("Victorian<br/>International Trade"),
      titleWidth = "40%"),
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(id = 'tabs',
        shinydashboard::menuItem("Home", tabName = "launchpad", selected = TRUE),
        shinydashboard::menuItem("Export explorer", tabName = "merch"),
        shinydashboard::menuItem("Balance of payments", tabName = "bop"),
        shinydashboard::menuItem("Notes", tabName = "methodology"),
        shinydashboard::menuItem("Accessibility", tabName = "accessibility")
      ),
      width = "250px"
    ),
    body = shinydashboard::dashboardBody(
      djprshiny::djpr_dash_theme(),
      shiny::tags$script("$('html').attr(\"lang\", \"en\")"),
      #tags$script("$('aside').attr(\"role\", \"navigation\")"),
      shiny::tags$script("$('section.content').attr(\"role\", \"main\")"),
      shinydashboard::tabItems(
        shinydashboard::tabItem("launchpad", page_launchpadUI('lp')),
        shinydashboard::tabItem("merch", page_merchUI()),
        shinydashboard::tabItem("bop", page_bopUI()),
        shinydashboard::tabItem("methodology", page_methodology()),
        shinydashboard::tabItem("accessibility",
                                fluidRow(
                                  shiny::tagList(
                                    h2(style = 'padding-top:80px',
                                       'axe.js Output'),
                                    includeScript('inst/js/axe.min.js'),
                                    includeScript('inst/js/run_axe.js'),
                                    shiny::tags$button(hreg = "#", onclick = "run_axe()",
                                           "Click to Run axe js"),
                                    shiny::tags$div(id = 'axe-results')
                                  )
                                )
                                )
      )
    )
  )
}
