

# setup db connection
pkgload::load_all()
library(dplyr)
library(dbplyr)
library(glue)
library(duckdb)
library(djprtradedash)


con <- duckdb::dbConnect(drv = duckdb::duckdb(),
                         db = "trade")

merch <- tbl(con, 'merch') # collect all here is 6 sec
merch_imp <- tbl(con, 'merch_imp')
supp_cy <- tbl(con, 'supp_cy')
supp_fy <- tbl(con, 'supp_fy')
bop <- tbl(con, 'bop')

merch_dates <- merch |>
  summarise(
    min = min(date, na.rm = TRUE),
    max = max(date, na.rm = TRUE)
    ) |>
  collect() |>
  mutate(across(everything(), as.Date))

merch_sitc <- merch |>
  summarize(sitc = distinct(sitc)) |>
  collect() |> pull()

merch_country_dest <- merch |>
  summarize(sitc = distinct(country_dest)) |>
  collect() |> pull()

in_global <- ls()

app()



