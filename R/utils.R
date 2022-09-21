# Transpose and string split factor levels
tstrsplit_factor <- function(fac, split){
  if(!inherits(fac, "factor")){ fac <- factor(fac)}
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



# Generic table generation fun
djpr_table <- function(df, first_col_header = TRUE){
  # Table container
  shiny::tags$table(
    class = "djprTable",
    # Header row
    shiny::tags$thead(
      shiny::tags$tr(lapply(colnames(df), function(x) shiny::tags$th(scope = "col", x)))
    ),
    # Table body
    shiny::tags$tbody(
      apply(df, 1, function(x) {
        shiny::tags$tr(
          c(
            list(
              if(first_col_header) {
                shiny::tags$th(scope = "row", x[[1]])
              } else {
                shiny::tags$td(x[[1]])
              }
            ),
            lapply(x[2:length(x)], function(y) shiny::tags$td(y))
          )
        )
      }
      )
    )
  )
}



round2 <- function (x, digits = 0)
{
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z/10^digits
  z * posneg
}









