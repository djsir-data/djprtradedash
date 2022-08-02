

# Load objects from this project
pkgload::load_all(".")

# Establish database connections
load_tabs()

# Start shinyapp
shiny::shinyApp(ui = ui(), server = server)


