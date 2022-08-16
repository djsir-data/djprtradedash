test_that("read_merch() works", {
  skip_if_offline()
  skip_on_ci()

  m <- read_merch()

  expect_s3_class(m, "data.frame")
  expect_type(m$value, "integer")
  expect_length(m, 9)
  expect_gte(nrow(m), 5000)

  m_imp <- read_merch(series = "import")

  expect_s3_class(m_imp, "data.frame")
  expect_type(m_imp$value, "integer")
  expect_length(m_imp, 9)
  expect_gte(nrow(m_imp), 5000)

})
