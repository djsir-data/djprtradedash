test_that("read_supp('cy') produce expected output", {
  all_cy <- read_supp()
  expect_type(all_cy, "list")
  expect_s3_class(all_cy, "tbl_df")
  expect_length(all_cy, 5)
  expect_gte(nrow(all_cy), 100000)
  expect_type(all_cy$value, "double")
  expect_identical(all_cy, read_supp("cy"))
})

test_that("read_supp('fy') produces expected output", {
  all_fy <- read_supp(format = "fy")
  expect_type(all_fy, "list")
  expect_s3_class(all_fy, "tbl_df")
  expect_length(all_fy, 5)
  expect_gte(nrow(all_fy), 100000)
  expect_type(all_fy$value, "double")
})

test_that("read_supp() downloads a single table when requested", {
  t1 <- read_supp("fy", table_no = 1)
  expect_gte(nrow(t1), 500)
  expect_identical(length(unique(t1$abs_series)), 1L)
})

test_that("read_supp() leaves files in the expected place", {
  mydir <- file.path(tempdir(), "test_read_supp")
  dir.create(mydir)
  on.exit(unlink(mydir))

  expect_length(list.files(mydir), 0)
  t1 <- read_supp("cy", 1, path = mydir)
  expect_gte(length(list.files(mydir)), 1)
})

test_that("table 9 is ignored by read_supp()", {
  t9 <- read_supp(table_no = 9)
  expect_length(t9, 0)
  expect_equal(nrow(t9), 0)
  expect_identical(read_supp(table_no = c(3, 9)),
                   read_supp(table_no = 3))
})
