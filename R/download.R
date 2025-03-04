# Data download functions

#' URLs for input data
download_urls <- data.table::fread("inst/download_urls.csv")

#' Process a predefined file set
#'
#' Handles downloading, caching and cleanup behind the scenes.
#'
#' @param fileset Set of files to process
#' @param process Callback `function(paths) {...}` to process files
#' @param ... Other arguments passed to `process(paths, ...)`
#' @returns Result of `process(paths, ...)`
with_fileset <- function(fileset, process = \(paths) { paths }, ...) {
  urls <- download_urls[set %in% fileset]
  if(nrow(urls) == 0) {
    stop(sprintf(
      "Unrecognised fileset `%s` -- should be one of: %s"
      ,toString(fileset)
      ,toString(download_urls$set)
    ))
  }

  paths <- vapply(urls$url, download_file, character(1), dir=tempdir())
  res <- NULL
  tryCatch({
    res <- process(paths, ...)
  }, finally = {
    if(config_dev()) message("Cleaning: ", toString(paths))
    lapply(paths, unlink)
  })
  res
}

#' Download a file from the internet
#'
#' For easier development, uses download cache if `config_download_cache()` is
#' not empty.
#'
#' @returns Path of downloaded file.
download_file <- function(url, dir) {
  dest <- file.path(dir, basename(url))

  cache_dir <- config_download_cache()
  cache_path <- file.path(cache_dir, basename(dest))
  if(cache_dir != "" && cache_path != dest && file.exists(cache_path)) {
    file.copy(cache_path, dest, overwrite = TRUE)
    return(dest)
  }

  if(config_dev()) {
    message(sprintf("Downloading: %s -> %s", url, dest))
  }
  res <- httr::GET(url, httr::write_disk(dest, overwrite = TRUE))
  status <- httr::http_status(res)
  if(status$category != "Success") {
    stop(sprintf("Download failed with message: %s", status$message))
  }

  if(cache_dir != "" && cache_path != dest) {
    # Seed the cache for next time
    file.copy(dest, cache_path, overwrite = TRUE)
  }

  dest
}
