
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
#>    exports_imports indicator goods_services state date       value series_id unit 
#>    <chr>           <chr>     <chr>          <chr> <date>     <dbl> <chr>     <chr>
#>  1 Exports         Current ~ Goods          New ~ 2011-09-01 11349 A8509274~ $ Mi~
#>  2 Exports         Current ~ Goods          Vict~ 2011-09-01  5756 A8509275~ $ Mi~
#>  3 Exports         Current ~ Goods          Quee~ 2011-09-01 14302 A8509270~ $ Mi~
#>  4 Exports         Current ~ Goods          Sout~ 2011-09-01  2959 A8509267~ $ Mi~
#>  5 Exports         Current ~ Goods          West~ 2011-09-01 31872 A8509275~ $ Mi~
#>  6 Exports         Current ~ Goods          Aust~ 2011-09-01     3 A8509274~ $ Mi~
#>  7 Exports         Current ~ Services       New ~ 2011-09-01  5832 A8509274~ $ Mi~
#>  8 Exports         Current ~ Services       Vict~ 2011-09-01  3280 A8509271~ $ Mi~
#>  9 Exports         Current ~ Services       Quee~ 2011-09-01  2440 A8509268~ $ Mi~
#> 10 Exports         Current ~ Services       Sout~ 2011-09-01   574 A8509270~ $ Mi~
#> # ... with 5,126 more rows
```

Import ABS merchandise exports data with `read_merch()`:

``` r
read_merch(min_date = as.Date("2021-01-01"),
                          max_date = as.Date("2021-02-01"))
#> Loading merchandise trade from local file:
#> C:\Users\victaxc\AppData\Local\Temp\Rtmpu4hOdV/abs_merch_2021-01-01_2021-02-01.xml
#> # A tibble: 40,757 x 7
#>    date       country_dest           sitc_rev3  sitc_rev3_code origin unit   value
#>    <date>     <chr>                  <chr>      <chr>          <chr>  <chr>  <dbl>
#>  1 2021-01-01 Austria                Agricultu~ 721            Austr~ 000s    3.15
#>  2 2021-01-01 Bangladesh             Agricultu~ 721            Austr~ 000s   24.0 
#>  3 2021-01-01 Belgium                Agricultu~ 721            Austr~ 000s   36.3 
#>  4 2021-01-01 Brazil                 Agricultu~ 721            Austr~ 000s    4.45
#>  5 2021-02-01 Brazil                 Agricultu~ 721            Austr~ 000s    8.54
#>  6 2021-01-01 Canada                 Agricultu~ 721            Austr~ 000s  179.  
#>  7 2021-01-01 China                  Agricultu~ 721            Austr~ 000s  166.  
#>  8 2021-02-01 China                  Agricultu~ 721            Austr~ 000s   85.6 
#>  9 2021-02-01 Christmas Island       Agricultu~ 721            Austr~ 000s   17.0 
#> 10 2021-02-01 Cocos (Keeling) Island Agricultu~ 721            Austr~ 000s    5.37
#> # ... with 40,747 more rows
```

Import ABS International Trade Supplementary Information with
`read_supp()`:

``` r
read_supp("cy", 3)
#> File downloaded in C:\Users\victaxc\AppData\Local\Temp\Rtmpu4hOdV/cy_3/All%20data%20cubes.zip
#> # A tibble: 9,702 x 5
#>    item                     year  value subset abs_series                         
#>    <chr>                    <chr> <dbl> <chr>  <chr>                              
#>  1 Manufacturing services ~ 1999     NA NSW    Table 3.1 International Trade in S~
#>  2 Maintenance and repair ~ 1999     29 NSW    Table 3.1 International Trade in S~
#>  3 Transport                1999   2724 NSW    Table 3.1 International Trade in S~
#>  4 Passenger (b)            1999   1563 NSW    Table 3.1 International Trade in S~
#>  5 Freight                  1999    235 NSW    Table 3.1 International Trade in S~
#>  6 Other                    1999    728 NSW    Table 3.1 International Trade in S~
#>  7 Postal and courier serv~ 1999    198 NSW    Table 3.1 International Trade in S~
#>  8 Travel                   1999   5132 NSW    Table 3.1 International Trade in S~
#>  9 Business                 1999    623 NSW    Table 3.1 International Trade in S~
#> 10 Personal                 1999   4509 NSW    Table 3.1 International Trade in S~
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
