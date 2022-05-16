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




# ABS Balance of Payments
bop <- read_bop()




# List data with names
out <- list(
  'merch'     = merch,
  'merch_imp' = merch_imp,
  'supp_cy'   = supp_cy,
  'supp_fy'   = supp_fy,
  'bop'       = bop
  )




# Connect to database
drv <- duckdb::duckdb()
con <- duckdb::dbConnect(drv, "trade_database.duckdb")




# Add csv data to database
mapply(
  FUN       = DBI::dbWriteTable,
  conn      = list(con),
  name      = names(out),
  value     = out,
  overwrite = TRUE
)




# Disconnect from database
duckdb::dbDisconnect(con)


