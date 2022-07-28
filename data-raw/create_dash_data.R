# Updates data from source and recompiles database


# Load required packages & set options
pkgload::load_all()




# Merchandise trade data
merch     <- read_merch(series = "export")
merch_imp <- read_merch(series = "import")




# Ensure merch files are data tables and filter for Victoria (to be reoved)
data.table::setDT(merch)
data.table::setDT(merch_imp)

merch <- merch[origin == "Victoria"]
merch_imp <- merch_imp[dest == "Victoria"]




# ABS International Trade Supplementary Information data
supp_cy <- read_supp("cy")
supp_fy <- read_supp("fy")




# Service data
services <- read_services()




# ABS Balance of Payments
bop <- read_bop()




# List data with names
out <- list(
  'merch'         = merch,
  'merch_imp'     = merch_imp,
  'supp_cy'       = supp_cy,
  'supp_fy'       = supp_fy,
  'bop'           = bop,
  'service_trade' = services
  )




# Connect to database
if(!exists("con")) load_tabs()




# Add csv data to database
mapply(
  FUN       = pool::dbWriteTable,
  conn      = list(con),
  name      = names(out),
  value     = out,
  overwrite = TRUE
)



