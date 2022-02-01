pkgload::load_all()
library(lubridate)
library(dplyr)

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
