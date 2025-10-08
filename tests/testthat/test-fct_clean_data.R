test_that("function returns full df when db is valid", {
  test_df <- test_fixture_riverfly()[[1]]

  testthat::expect_equal(
    clean_data(
      input_df = test_df,
      col_name_start = "organisation",
      col_name_end = "leeches",
      sample_site = "sampling_site",
      acceptable_site_orgs = data.frame(
        identifiers = c("Org1 SiteA", "Org2 SiteB", "Org3 SiteD")
      ),
      data_type_name = "TestType"
    ),
    test_df[, c(1, 3, 2, 4, 5, 6)]
  )
})

test_that("function returns no error when db is valid", {
  test_df <- test_fixture_riverfly()[[1]]

  testthat::expect_no_error(
    clean_data(
      input_df = test_df,
      col_name_start = "organisation",
      col_name_end = "leeches",
      sample_site = "sampling_site",
      acceptable_site_orgs = data.frame(
        identifiers = c("Org1 SiteA", "Org2 SiteB", "Org3 SiteD")
      ),
      data_type_name = "TestType"
    )
  )
})

test_that("function catches an invalid site and organisation combination", {
  test_df <- test_fixture_riverfly()[[1]]

  testthat::expect_warning(clean_data(
    input_df = test_df,
    col_name_start = "organisation",
    col_name_end = "leeches",
    sample_site = "sampling_site",
    acceptable_site_orgs = data.frame(
      identifiers = c("Org1 SiteA", "Org2 SiteB", "Org3 SiteD")
    ),
    data_type_name = "TestType"
  ))
})

test_that("function catches an entry with a blank site", {
  test_df <- test_fixture_riverfly()[[1]]
  test_df$sampling_site[1] <- ""

  testthat::expect_warning(clean_data(
    input_df = test_df,
    col_name_start = "organisation",
    col_name_end = "leeches",
    sample_site = "sampling_site",
    acceptable_site_orgs = data.frame(
      identifiers = c("Org1 SiteA", "Org2 SiteB", "Org3 SiteD")
    ),
    data_type_name = "TestType"
  ))
})

test_that("function catches an entry with a duplicate site and timestamp", {
  test_df <- test_fixture_riverfly()[[1]]
  test_df$survey_date[2] <- as.Date("2023-10-01")

  testthat::expect_warning(clean_data(
    input_df = test_df,
    col_name_start = "organisation",
    col_name_end = "leeches",
    sample_site = "sampling_site",
    acceptable_site_orgs = data.frame(
      identifiers = c("Org1 SiteA", "Org2 SiteB", "Org3 SiteD")
    ),
    data_type = "TestType"
  ))
})
