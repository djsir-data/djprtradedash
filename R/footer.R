footer <- function(...){
  fluidRow(
    div(
      class = "col-xl-2 mx-auto",
      div(
        class = "box",
        style = "background-color:var(--twilight); padding: 15px; text-align:center;",
        p(
          style = "margin:0;",
          a(
            href = "#",
            class = "disclaimerLink",
            "Disclaimer"
          ),
          " | ",
          a(
            href = "https://github.com/djpr-data/djprtradedash",
            target="_blank",
            "Github"
          )
        )
      )
    )
  )
}
