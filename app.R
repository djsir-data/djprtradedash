# if(exists("con")) duckdb::dbDisconnect(con, shutdown = T)
# djprtradedash:::kill_cache()
pkgload::load_all(".")
app()



