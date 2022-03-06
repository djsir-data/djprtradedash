

# setup db connection
pkgload::load_all()
library(dplyr)
library(dbplyr)
library(glue)
library(duckdb)
library(djprtradedash)


con <- duckdb::dbConnect(drv = duckdb::duckdb(),
                         db = "trade_database.duckdb")

merch <- dplyr::tbl(con, 'merch') # collect all here is 6 sec
merch_imp <- dplyr::tbl(con, 'merch_imp')
supp_cy <- dplyr::tbl(con, 'supp_cy')
supp_fy <- dplyr::tbl(con, 'supp_fy')
bop <- dplyr::tbl(con, 'bop')

merch_dates <- merch %>%
  dplyr::summarise(
    min = min(date, na.rm = TRUE),
    max = max(date, na.rm = TRUE)
    ) %>%
  dplyr::collect()  %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.Date))

merch_sitc <- merch  %>%
  dplyr::summarize(sitc = distinct(sitc)) %>%
  dplyr::collect() %>%
  dplyr::pull()

merch_country_dest <- merch %>%
  dplyr::summarize(sitc = dplyr::distinct(country_dest))  %>%
  dplyr::collect() %>%
  dplyr::pull()

in_global <- ls()
message(paste0(in_global, collapse = "\n"))

app()



