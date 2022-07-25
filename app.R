# if(exists("con")) pool::poolClose(con)
# djprtradedash:::kill_cache()
pkgload::load_all(".")
app()



