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
load_tabs()




# Get validation table & check if out tables match
val <- tbl(con, "tablevalidation") %>%
  filter(name %in% !!names(out)) %>%
  collect() %>%
  group_by(name) %>%
  filter(timestamp == max(timestamp, na.rm = TRUE)) %>%
  ungroup()

matched_tables <- intersect(
  val$name,
  names(out)
)

hash_matches <- sapply(matched_tables, function(x){
  val$rlanghash[val$name == x] == rlang::hash(out[[x]])
})




# Update out to remove tables with matched hashes
if(length(hash_matches) > 0){
  out <- out[!(names(out) %in% matched_tables[hash_matches])]
}





# Add data to database
mapply(
  FUN       = pool::dbWriteTable,
  conn      = list(con),
  name      = names(out),
  value     = out,
  overwrite = TRUE
)




# Update hashes
if(length(out) > 0){

  new_hashes <- data.frame(
    name = names(out),
    timestamp = Sys.time(),
    rlanghash = sapply(out, rlang::hash)
  )

  pool::dbAppendTable(con, "tablevalidation", new_hashes)
}




# Clear environment and disconnect
pool::poolClose(con)
rm(list = ls())




