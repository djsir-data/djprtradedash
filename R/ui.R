
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
        shinydashboard::menuItem("Goods exports", tabName = "merch", selected = TRUE),
        shinydashboard::menuItem("Service Exports", tabName = "services"),
        shinydashboard::menuItem("Trade overview", tabName = "launchpad"),
        shinydashboard::menuItem("Balance of payments", tabName = "bop"),
        shinydashboard::menuItem("FAQ & sources", tabName = "methodology"),
        shinydashboard::menuItem("Disclaimer", tabName = "disclaimer") %>%
          shiny::tagAppendAttributes(
            style = "display:none;"
          )
      ),
      width = "250px"
    ),
    body = shinydashboard::dashboardBody(
      tags$head(
        # Bootstrap 5 for xl columns
        tags$link(
          href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
          rel = "stylesheet",
          integrity = "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
          crossorigin = "anonymous"
        ),
        # Gloabl vic css
        tags$link(href = "globalvic.css", rel = "stylesheet"),
        # Launchpad links (client side execution)
        shiny::tags$script("$(document).ready(function(){$('a.merchLink').click(function(){$('.sidebar-menu a[data-value=\"merch\"]').trigger('click');})});"),
        shiny::tags$script("$(document).ready(function(){$('a.bopLink').click(function(){$('.sidebar-menu a[data-value=\"bop\"]').trigger('click');})});"),
        shiny::tags$script("$(document).ready(function(){$('a.servicesLink').click(function(){$('.sidebar-menu a[data-value=\"services\"]').trigger('click');})});"),
        shiny::tags$script("$(document).ready(function(){$('a.disclaimerLink').click(function(){$('.sidebar-menu a[data-value=\"disclaimer\"]').trigger('click');})});"),
        # Resize highcharts on sidebar collapse
        shiny::tags$script(src = "sidebar_chart_resize.JS")
        ),
      shiny::tags$script("$('html').attr(\"lang\", \"en\")"),
      shiny::tags$script("$('section.content').attr(\"role\", \"main\")"),
      shinydashboard::tabItems(
        shinydashboard::tabItem("launchpad", page_launchpadUI('lp')),
        shinydashboard::tabItem("merch", page_merchUI()),
        shinydashboard::tabItem("bop", page_bopUI()),
        shinydashboard::tabItem("services", page_servicesUI()),
        shinydashboard::tabItem("methodology", page_methodology()),
        shinydashboard::tabItem("disclaimer", page_disclaimerUI())
      )
    )
  )
}
