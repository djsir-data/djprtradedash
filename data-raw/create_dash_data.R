# Updates data from source and recompiles database


# Load required packages & set options
pkgload::load_all()




# Merchandise trade data
merch     <- read_merch(series = "export")
merch_imp <- read_merch(series = "import")




# Service data
services <- read_services()




# ABS Balance of Payments
bop <- read_bop()




# List data with names
out <- list(
  'merch'         = merch,
  'merch_imp'     = merch_imp,
  'bop'           = bop,
  'service_trade' = services
  )




# Connect to database
if(!exists("con")) load_tabs()




# Add data to database
mapply(
  FUN       = pool::dbWriteTable,
  conn      = list(con),
  name      = names(out),
  value     = out,
  overwrite = TRUE
)



