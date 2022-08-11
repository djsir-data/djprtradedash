# Transpose and string split factor levels
tstrsplit_factor <- function(fac, split){
  lev <- levels(fac)
  ind <- as.integer(fac)
  split <- data.table::tstrsplit(lev, split = split)
  lapply(split, function(x) x[ind])
}

# Trycatch infix function
`%iferror%` <- function(a, b) tryCatch({a}, error = function(e){b})


# Column shim
column <- function(width, ...){
  colClass <- paste0("col-xl-", width)
  shiny::div(class = colClass, ...)
}

to_col_xl <- function(x){
  x$attribs$class <- paste0(
    "col-xl-",
    substr(x$attribs$class, 8, nchar(x$attribs$class))
    )
  return(x)
}












