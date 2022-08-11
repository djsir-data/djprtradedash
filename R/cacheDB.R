cacheDB <- function(...){

  load_tabs()

  message("Downloading data")
  merch <- collect(merch)
  message("merch downloaded")
  merch_imp <- collect(merch_imp)
  message("merch_imp downloaded")
  bop <- collect(bop)
  message("bop downloaded")
  service_trade <- collect(service_trade)
  message("service_trade downloaded")

  save_list <- c(
    "merch",
    "merch_imp",
    "bop",
    "service_trade",
    "merch_dates",
    "merch_sitc_lu",
    "bop_dates",
    "service_dates",
    "merch_country_dest",
    "service_categories"
    )

  save(list = save_list, file = "./DBcache.rData")

}
