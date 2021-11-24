pkgload::load_all()
library(lubridate)
library(djprtradedata)
library(dplyr)

options("timeout" = 180)

# Merchandise trade data ------
# SET MINIMUM YEAR TO INCLUDE IN MERCH DATA
min_merch_year <- 2010

# Create lookup table of merchandise trade data -----
lookup <- create_merch_lookup()

# Get merchandise trade data -----
# We cannot download all merch trade data in one go; we need to
# get 1 year at a time.
#
# Create vectors of dates defining the first & final month of each
# year from 1995 to last year PLUS the start of this year to
# the current date
last_year <- year(Sys.Date()) - 1

prior_years <- seq.Date(
  from = ymd(paste0(last_year, "-12-01")),
  to = ymd(paste0(min_merch_year, "-12-01")),
  by = "-1 year"
)

merch_end_dates <- c(Sys.Date(), prior_years)
merch_start_dates <- floor_date(merch_end_dates, "year")

# We now iterate over our dates - call `read_merch()` multiple
# times, once per year worth of data
merch <- purrr::map2_dfr(
  .x = merch_end_dates,
  .y = merch_start_dates,
  .f = ~ read_merch(
    path = here::here(
      "data-raw",
      "abs_merch_raw"
    ),
    min_date = .y,
    max_date = .x,
    merch_lookup = lookup
  )
) %>%
  dplyr::arrange(.data$date)

merch <- merch %>%
  mutate_if(is.character, as.factor)

# Check that `merch` is as we expect
merch_tests <- c(
  inherits(merch, "tbl_df"),
  max(merch_end_dates) - max(merch$date) < 180,
  min(merch_start_dates) == min(merch$date)
)

stopifnot(all(merch_tests))

# ABS International Trade Supplementary Information data ------
supp_cy <- read_supp("cy")
supp_fy <- read_supp("fy")

use_data(
  merch,
  supp_cy,
  supp_fy,
  internal = TRUE,
  overwrite = TRUE,
  version = 3
)
