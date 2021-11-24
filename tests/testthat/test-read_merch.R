test_that("read_merch() works", {
  skip_if_offline()

  dir <- tempdir()
  m <- read_merch(
    min_date = Sys.Date() - 90,
    path = dir
  )

  expect_s3_class(m, "tbl_df")
  expect_type(m$value, "double")
  expect_length(m, 7)
  expect_gte(nrow(m), 10000)
  expect_message(
    read_merch(
      min_date = Sys.Date() - 90,
      path = dir
    ),
    "local file"
  )
  expect_message(
    read_merch(
      min_date = Sys.Date() - 90,
      path = dir,
      check_local = FALSE
    ),
    "Downloading"
  )
})
