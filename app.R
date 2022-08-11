

# Load objects from this project
pkgload::load_all(".")

# Load environment variables (if custom file exists)
if(file.exists(".renviron")) readRenviron(".renviron")

# Establish database connections
load_tabs()

# Start shinyapp
shiny::shinyApp(ui = ui(), server = server)


