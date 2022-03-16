options(rsconnect.max.bundle.size = 5e+9)
rsconnect::deployApp(
  appName = "trade-henry",
  forceUpdate = TRUE,
  logLevel = "verbose",
  appFiles = c(
    list.files("R/", full.names = T, recursive = T),
    list.files("app-cache/", full.names = T, recursive = T),
    list.files("rsconnect/", full.names = T, recursive = T),
    list.files(".Rproj.user/", full.names = T, recursive = T),
    "app.R",
    "DESCRIPTION",
    "djprtradedash.Rproj",
    "NAMESPACE",
    "trade_database.duckdb",
    "trade_database.duckdb.wal"
  )
)
