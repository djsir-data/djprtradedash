

# Write DB connection environment variables
if(
  !is.null(getOption("PG_READ_OPEN_USER")) &
  !is.null(getOption("PG_READ_OPEN_PW")) &
  !file.exists(".renviron")
){
  writeLines(
    con = ".renviron",
    text = c(
      paste("PG_READ_OPEN_USER", "=", getOption("PG_READ_OPEN_USER")),
      paste("PG_READ_OPEN_PW", "=", getOption("PG_READ_OPEN_PW"))
    )
  )
} else {
  stop("database credentials not present as environment variable")
}

# Publish App
rsconnect::deployApp(
  appName = "gv-test",
  forceUpdate = TRUE
)


# Remove renviron file
unlink(".renviron")
