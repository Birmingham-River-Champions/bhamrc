test_df <- test_fixture_riverfly()[[1]]
locations_name <- "BRC_Sampling_Locs"

test_that("function returns full df when db is valid", {
  testthat::expect_equal(
    clean_data(
      input_df = test_df,
      col_name_start = "organisation",
      col_name_end = "stonefly_plecoptera",
      sample_site = "sampling_site",
      locations_name = locations_name,
      data_type_name = "Urban Riverfly"
    ),
    test_df[, c(1, 3, 2, 4:18)]
  )
})

test_that("function returns no error when db is valid", {
  testthat::expect_no_error(
    clean_data(
      input_df = test_df,
      col_name_start = "organisation",
      col_name_end = "stonefly_plecoptera",
      sample_site = "sampling_site",
      locations_name = locations_name,
      data_type_name = "Urban Riverfly"
    )
  )
})

test_that("function catches an invalid site and organisation combination", {
  wrong_org_df <- test_df
  wrong_org_df$sampling_site[1] <- "SiteB"
  testthat::expect_warning(clean_data(
    input_df = wrong_org_df,
    col_name_start = "organisation",
    col_name_end = "stonefly_plecoptera",
    sample_site = "sampling_site",
    locations_name = locations_name,
    data_type_name = "Urban Riverfly"
  ))
})

test_that("function catches an entry with a blank site", {
  blank_df <- test_df
  blank_df$sampling_site[1] <- ""

  testthat::expect_warning(clean_data(
    input_df = blank_df,
    col_name_start = "organisation",
    col_name_end = "stonefly_plecoptera",
    sample_site = "sampling_site",
    locations_name = locations_name,
    data_type_name = "Urban Riverfly"
  ))
})

test_that("function catches an entry with a duplicate site and timestamp", {
  dupe_df <- test_df
  dupe_df$survey_date[2] <- as.Date("2023-10-01")

  testthat::expect_warning(clean_data(
    input_df = dupe_df,
    col_name_start = "organisation",
    col_name_end = "stonefly_plecoptera",
    sample_site = "sampling_site",
    locations_name = locations_name,
    data_type = "Urban Riverfly"
  ))
})
