tstrsplit_factor <- function(fac, split){
  lev <- levels(fac)
  ind <- as.integer(fac)
  split <- data.table::tstrsplit(lev, split = split)
  lapply(split, function(x) x[ind])
}
