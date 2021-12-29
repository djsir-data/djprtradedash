
<!-- README.md is generated from README.Rmd. Please edit that file -->

# djprtradedash

<!-- badges: start -->

[![R-CMD-check](https://github.com/djpr-data/djprtradedash/workflows/R-CMD-check/badge.svg)](https://github.com/djpr-data/djprtradedash/actions)
[![Codecov test
coverage](https://codecov.io/gh/djpr-data/djprtradedash/branch/main/graph/badge.svg)](https://app.codecov.io/gh/djpr-data/djprtradedash?branch=main)

<!-- badges: end -->

The [DJPR Trade Dashboard](https://djpr-spp.shinyapps.io/djprtradedash/)
provides a convenient way for users to browse publicly-accessible data
about Victorian exports and imports.

## Installing the package

Install from GitHub with:

``` r
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}

remotes::install_github("djpr-data/djprtradedash", dependencies = TRUE)
```

``` r
library(djprtradedash)
```

## Data functions

`djprtradedash` contains functions to download and tidy trade data from
publicly-available sources such as the ABS.

Import ABS balance of payments data by State/Territory with
`read_bop()`:

``` r
read_bop()
#> # A tibble: 5,136 x 8
#>    exports_imports indicator     goods_services state                        date       value series_id  unit      
#>    <chr>           <chr>         <chr>          <chr>                        <date>     <dbl> <chr>      <chr>     
#>  1 Exports         Current Price Goods          New South Wales              2011-09-01 11349 A85092741C $ Millions
#>  2 Exports         Current Price Goods          Victoria                     2011-09-01  5756 A85092752K $ Millions
#>  3 Exports         Current Price Goods          Queensland                   2011-09-01 14302 A85092700J $ Millions
#>  4 Exports         Current Price Goods          South Australia              2011-09-01  2959 A85092678X $ Millions
#>  5 Exports         Current Price Goods          Western Australia            2011-09-01 31872 A85092753L $ Millions
#>  6 Exports         Current Price Goods          Australian Capital Territory 2011-09-01     3 A85092743J $ Millions
#>  7 Exports         Current Price Services       New South Wales              2011-09-01  5832 A85092746R $ Millions
#>  8 Exports         Current Price Services       Victoria                     2011-09-01  3280 A85092714W $ Millions
#>  9 Exports         Current Price Services       Queensland                   2011-09-01  2440 A85092681L $ Millions
#> 10 Exports         Current Price Services       South Australia              2011-09-01   574 A85092701K $ Millions
#> # ... with 5,126 more rows
```

Import ABS merchandise exports data with `read_merch()`:

``` r
read_merch(min_date = as.Date("2021-01-01"),
                          max_date = as.Date("2021-02-01"))
#> Downloading NSW export merchandise trade data from 2021-01 to 2021-02
#> Downloading VIC export merchandise trade data from 2021-01 to 2021-02
#> Downloading QLD export merchandise trade data from 2021-01 to 2021-02
#> Downloading SA export merchandise trade data from 2021-01 to 2021-02
#> Downloading WA export merchandise trade data from 2021-01 to 2021-02
#> Downloading TAS export merchandise trade data from 2021-01 to 2021-02
#> Downloading NT export merchandise trade data from 2021-01 to 2021-02
#> Downloading ACT export merchandise trade data from 2021-01 to 2021-02
#> Downloading NA export merchandise trade data from 2021-01 to 2021-02
#> Downloading REEXP export merchandise trade data from 2021-01 to 2021-02
#> # A tibble: 71,504 x 8
#>    date       country_dest             sitc_rev3                          sitc_rev3_code origin        unit  value export_import
#>    <date>     <chr>                    <chr>                              <chr>          <chr>         <chr> <dbl> <chr>        
#>  1 2021-02-01 Bhutan                   Commodities and transactions not ~ 9              Australian C~ 000s      3 export       
#>  2 2021-01-01 Hong Kong (SAR of China) Commodities and transactions not ~ 9              Australian C~ 000s    558 export       
#>  3 2021-01-01 Total                    Commodities and transactions not ~ 9              Australian C~ 000s    558 export       
#>  4 2021-02-01 Total                    Commodities and transactions not ~ 9              Australian C~ 000s    545 export       
#>  5 2021-02-01 United States of America Commodities and transactions not ~ 9              Australian C~ 000s    542 export       
#>  6 2021-01-01 Hong Kong (SAR of China) Gold coin whether or not legal te~ 95             Australian C~ 000s    558 export       
#>  7 2021-01-01 Hong Kong (SAR of China) Gold coin whether or not legal te~ 951            Australian C~ 000s    558 export       
#>  8 2021-01-01 Total                    Gold coin whether or not legal te~ 95             Australian C~ 000s    558 export       
#>  9 2021-01-01 Total                    Gold coin whether or not legal te~ 951            Australian C~ 000s    558 export       
#> 10 2021-02-01 Spain                    Measuring, checking, analysing an~ 874            Australian C~ 000s    534 export       
#> # ... with 71,494 more rows
```

Import ABS International Trade Supplementary Information with
`read_supp()`:

``` r
read_supp("cy", 3)
#> # A tibble: 9,702 x 5
#>    item                                                      year  value subset abs_series                                      
#>    <chr>                                                     <chr> <dbl> <chr>  <chr>                                           
#>  1 Manufacturing services on physical inputs owned by others 1999     NA NSW    Table 3.1 International Trade in Services, Cred~
#>  2 Maintenance and repair services n.i.e                     1999     29 NSW    Table 3.1 International Trade in Services, Cred~
#>  3 Transport                                                 1999   2724 NSW    Table 3.1 International Trade in Services, Cred~
#>  4 Passenger (b)                                             1999   1563 NSW    Table 3.1 International Trade in Services, Cred~
#>  5 Freight                                                   1999    235 NSW    Table 3.1 International Trade in Services, Cred~
#>  6 Other                                                     1999    728 NSW    Table 3.1 International Trade in Services, Cred~
#>  7 Postal and courier services (c)                           1999    198 NSW    Table 3.1 International Trade in Services, Cred~
#>  8 Travel                                                    1999   5132 NSW    Table 3.1 International Trade in Services, Cred~
#>  9 Business                                                  1999    623 NSW    Table 3.1 International Trade in Services, Cred~
#> 10 Personal                                                  1999   4509 NSW    Table 3.1 International Trade in Services, Cred~
#> # ... with 9,692 more rows
```

## Data objects

The file `data-raw/create_dash_data.R` uses the data functions (see
above) to download and tidy data. That data is then stored as internal
data objects in this package.

-   `merch` is merchandise exports data from `read_merch()`

-   `bop` is balance of payments data from `read_bop()`

-   `supp_cy` is supplementary calendar year trade information from
    `read_supp("cy")`

-   `supp_fy` is supplementary financial year trade information from
    `read_supp("fy")`
