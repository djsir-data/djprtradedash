test_that("lookup table is as expected", {
  expect_snapshot_value(create_merch_lookup(),
    style = "json2"
  )

  lookup <- create_merch_lookup()
  expect_type(lookup, "list")
  purrr::map(lookup,
    expect_s3_class,
    class = "tbl_df"
  )
})
