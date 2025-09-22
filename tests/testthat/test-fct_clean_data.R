library(testthat)
library(bhamrc)

test.df <- data.frame(
  organisation = c("Org1", "Org1", "Org2", "Org2", "Org3", "Org3"),
  survey_date = as.Date(c(
    "2023-10-01",
    "2023-10-05",
    "2023-10-02",
    "2023-10-03",
    "2023-10-04",
    "2023-10-04"
  )),
  sampling_site = c("SiteA", "SiteA", "SiteB", "SiteC", "SiteC", "SiteD"),
  other_col = c(1, 1, 2, 3, 4, 4)
)

test_that("function catches an invalid site and organisation combination", {
  testthat::expect_warning(clean_data(
    input_df = test.df,
    col_name_start = "sampling_site",
    col_name_end = "other_col",
    sample_site = "sampling_site",
    acceptable_site_orgs = data.frame(
      identifiers = c("Org1 SiteA", "Org2 SiteB", "Org3 SiteD")
    ),
    data_type = "TestType"
  ))
})

test_that("function catches an entry with a blank site", {
  test.df$sampling_site[1] <- ""
  testthat::expect_warning(clean_data(
    input_df = test.df,
    col_name_start = "sampling_site",
    col_name_end = "other_col",
    sample_site = "sampling_site",
    acceptable_site_orgs = data.frame(
      identifiers = c("Org1 SiteA", "Org2 SiteB", "Org3 SiteD")
    ),
    data_type = "TestType"
  ))
})

test_that("function catches an entry with a duplicate site and timestamp", {
  test.df$survey_date[2] <- as.Date("2023-10-01")
  testthat::expect_warning(clean_data(
    input_df = test.df,
    col_name_start = "sampling_site",
    col_name_end = "other_col",
    sample_site = "sampling_site",
    acceptable_site_orgs = data.frame(
      identifiers = c("Org1 SiteA", "Org2 SiteB", "Org3 SiteD")
    ),
    data_type = "TestType"
  ))
})
