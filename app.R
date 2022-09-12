

# Load objects from this project
pkgload::load_all(".")

# Load environment variables (if custom file exists)
if(file.exists(".renviron")) readRenviron(".renviron")

# Optional cached DB mode (use cacheDB() to create)
# Sys.setenv(R_CONFIG_ACTIVE = "local_cache")

# Define global variables available to UI and server
globals <- c(
  "merch_dates",
  "bop_dates",
  "merch_sitc_lu",
  "merch_country_dest",
  "service_dates",
  "service_categories"
)

lapply(globals, function(x){
  assign(x, readRDS(paste0("inst/", x, ".rds")), envir = .GlobalEnv)
})


# Start shinyapp
shiny::shinyApp(ui = ui(), server = server)
