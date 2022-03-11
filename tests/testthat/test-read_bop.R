test_that("read_bop() gets balance of payments data", {
  bop <- read_bop()
  expect_gte(nrow(bop), 100)
  expect_equal(length(bop), 8)
})
