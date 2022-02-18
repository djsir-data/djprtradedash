pkgload::load_all()
library(lubridate)
library(dplyr)
library(purrr)

options("timeout" = 180)

# Merchandise trade data ------
merch <- read_merch()
merch_imp <- read_merch(series = "import")

# ABS International Trade Supplementary Information data ------
supp_cy <- read_supp("cy")
supp_fy <- read_supp("fy")

# ABS Balance of Payments
bop <- read_bop()

usethis::use_data(
  merch,
  merch_imp,
  supp_cy,
  supp_fy,
  bop,
  internal = TRUE,
  overwrite = TRUE,
  version = 3
)



### save as CSV and load into duckdb
out <- list('merch' = merch,
             'merch_imp' = merch_imp,
             'supp_cy' = supp_cy,
             'supp_fy' = supp_fy,
             'bop' = bop)

walk(names(out), ~ write.csv(out[[.x]], paste0('data-raw/csv/', .x, '.csv'),
                             na = '',
                             row.names = FALSE))

drv <- duckdb()
con <- dbConnect(drv, 'trade')

walk(names(out), ~ duckdb_read_csv(con, .x, paste0('data-raw/csv/', .x, '.csv'), na.strings = '', nrow.check = 2e6))

dbDisconnect(con)


