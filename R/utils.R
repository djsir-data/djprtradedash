# Transpose and string split factor levels
tstrsplit_factor <- function(fac, split){
  lev <- levels(fac)
  ind <- as.integer(fac)
  split <- data.table::tstrsplit(lev, split = split)
  lapply(split, function(x) x[ind])
}


# Trycatch infix function
`%iferror%` <- function(a, b) tryCatch({a}, error = function(e){b})


# Assign multiple lazy data frames to global environement
assign_table_global <- function(con, tables){
  lapply(tables, function(tab){
    assign(tab, dplyr::tbl(con, tab), envir = .GlobalEnv)
  })
}


# Format dollar figures
dollar_stat <- function(stat){
  dplyr::case_when(
    stat > 1e10 ~ scales::dollar(
      stat / 1e09,
      accuracy = 1,
      suffix = "b"
    ),
    stat > 1e09 ~ scales::dollar(
      stat / 1e09,
      accuracy = 1.1,
      suffix = "b"
    ),
    stat > 1e07 ~ scales::dollar(
      stat / 1e06,
      accuracy = 1,
      prefix = "$",
      suffix = "m"
    ),
    stat > 1e06 ~ scales::dollar(
      stat / 1e06,
      accuracy = 1.1,
      prefix = "$",
      suffix = "m"
    ),
    stat > 1e04 ~ scales::dollar(
      stat / 1e03,
      accuracy = 1,
      suffix = "k"
    ),
    stat > 1e03 ~ scales::dollar(
      stat / 1e03,
      accuracy = 1.1,
      suffix = "k"
    ),
    TRUE ~ scales::dollar(
      stat,
      accuracy = 1
    )
  )
}


# Delete cache files
kill_cache <- function(...){
  unlink("./app-cache/*", recursive = TRUE, force = TRUE)
}


# Shiny components
bop_date_slider <- function(
  id,
  label = "Dates",
  min = bop_dates$min,
  max = bop_dates$max,
  value = c(bop_dates$min, bop_dates$max),
  width = "90%",
  timeFormat = "%b %Y",
  dragRange = TRUE,
  ticks = FALSE,
  ...
  ){
  shiny::sliderInput(
    shiny::NS(id, "dates"),
    label = "Dates",
    min = min,
    max = max,
    value = value,
    width = width,
    timeFormat = timeFormat,
    dragRange = dragRange,
    ticks = ticks,
    ...
  )
}

merch_date_slider <- function(
  id,
  label = "Dates",
  min = merch_dates$min,
  max = merch_dates$max,
  value = c(merch_dates$min, merch_dates$max),
  width = "90%",
  timeFormat = "%b %Y",
  dragRange = TRUE,
  ticks = FALSE,
  ...
){
  shiny::sliderInput(
    shiny::NS(id, "dates"),
    label = "Dates",
    min = min,
    max = max,
    value = value,
    width = width,
    timeFormat = timeFormat,
    dragRange = dragRange,
    ticks = ticks,
    ...
  )
}
