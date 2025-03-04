# Centralise configuration in one place

config_dev <- function() {
  Sys.getenv("ENV") == "dev"
}

config_download_cache <- function() {
  Sys.getenv("DOWNLOAD_CACHE")
}
