tstrsplit_factor <- function(fac, split){
  lev <- levels(fac)
  ind <- as.integer(fac)
  split <- data.table::tstrsplit(lev, split = split)
  lapply(split, function(x) x[ind])
}

validate_query <- function(query){
  if(!("list" %in% class(query))) return(NULL)
  names(query) <- tolower(names(query))
  if(!("country" %in% names(query))) return(NULL)
  return(query[["country"]][1])
}

`%iferror%` <- function(a, b) tryCatch({a}, error = function(e){b})

data_unavil_ggplot<- function(...){
  g <- ggplot2::ggplot(NULL, aes(1,1,label = paste0(..., collapse = ""))) +
    ggplot2::theme_void() +
    ggplot2::geom_text(size = 5, colour = djprtheme::djpr_blue) +
    ggplot2::theme(panel.background = element_rect(fill = "#bfbfbf"))
  djprtheme::gg_font_change(g, "Roboto")
}

error_safe_plotfun <- function(plotfun){
    function(...) plotfun(...) %iferror% data_unavil_ggplot("Data unavailable")
}


#Unused
append_header <- function(...){
  htmltools::tags$script(
    HTML(
    "var header = $('.navbar > .container-fluid > .navbar-collapse');
     header.append('",HTML(...),"')"
    )
  )
}
