pkgload::load_all(".")

# testing overwrite merch with db connection


# setup db connection
library(pool)
library(dplyr)
library(dbplyr)
library(glue)

db_pool <- dbPool(
  drv = duckdb::duckdb(),
  db = "trade"
)



merch <- tbl(db_pool, 'merch') # collect all here is 6 sec
merch_imp <- tbl(db_pool, 'merch_imp')
supp_cy <- tbl(db_pool, 'supp_cy')
supp_fy <- tbl(db_pool, 'supp_fy')
bop <- tbl(db_pool, 'bop')

app()


