

# Load objects from this project
pkgload::load_all(".")

# Load environment variables (if custom file exists)
if(file.exists(".renviron")) readRenviron(".renviron")

# Optional cached DB mode (use cacheDB() to create)
# Sys.setenv(R_CONFIG_ACTIVE = "local_cache")

# Establish database connections
load_tabs()

# Start shinyapp
shiny::shinyApp(ui = ui(), server = server)


