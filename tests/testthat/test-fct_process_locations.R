test_that("process_locations works", {
  test_df <- test_fixture_riverfly()
  test_locs <- test_df$riverfly_locs

  expect_equal(
    process_locations(test_df$riverfly_locs),
    test_locs
  )
})
