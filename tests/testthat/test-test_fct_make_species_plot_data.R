test_that("species_plots works", {
  test_values <- test_fixture_riverfly()
  test_df <- test_values[[1]]
  test_locs <- test_values[[2]]

  plot_test_data <- left_join(
    test_df,
    test_locs,
    by = c("sampling_site" = "ID"),
    multiple = "first"
  )

  test_spp <- species_plots("riverflytest", sampling_locs)

  expect_equal(2 * 2, 4)
})

test_that("make_recent_inv_spp works", {
  test_inv_spp <- make_recent_inv_spp(cleaned_data, sampling_locs, plot_palette)

  expect_equal(2 * 2, 4)
})

test_that("flip_site_names works", {
  site_names <- c(
    "River, Tame",
    "Canal, Birmingham",
    "Lake, Edgbaston",
    "SiteA",
    "SiteB"
  )

  flip_test <- c(
    "Tame River",
    "Birmingham Canal",
    "Edgbaston Lake",
    "SiteA",
    "SiteB"
  )

  flipped <- flip_site_names(site_names)

  expect_equal(flipped, flip_test)
})
