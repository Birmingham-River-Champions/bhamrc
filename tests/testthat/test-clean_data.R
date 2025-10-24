test_data <- test_fixture_riverfly()
test_df <- test_data[[1]]
locations_name <- "BRC_Sampling_Locs"

test_that("function returns full df when db is valid", {
  # Create a temporary database and populate it with the test data
  cleaned_df <- test_df |>
    #First select columns from col_name_start -> col_name_end
    dplyr::select(
      organisation:stonefly_plecoptera
    ) |>
    ###Remove data uploads that included no site identifier
    dplyr::filter(sampling_site != "")

  locations <- test_data[[5]]
  acceptable_site_orgs <- acceptable_locs(locations)

  # Filter out any observations for which the sampling site and organisation don't match what is expected
  wrong_org <- cleaned_df |>
    dplyr::mutate(
      site_orgs = paste(organisation, sampling_site)
    ) |>
    dplyr::filter(grepl("Urban Riverfly", data_type)) |>
    dplyr::filter(!(site_orgs %in% acceptable_site_orgs$identifiers))

  ## Filter out rows where the sampling site and organisation don't match
  correct_org_df <- cleaned_df |>
    dplyr::mutate(
      site_orgs = paste(organisation, sampling_site)
    ) |>
    dplyr::filter(site_orgs %in% acceptable_site_orgs$identifiers) |>
    dplyr::select(-site_orgs)

  #Also check if there are duplicates, each sampling site + timestamp should be unique
  deduped_df <- correct_org_df |>
    dplyr::distinct(
      survey_date,
      sampling_site,
      .keep_all = TRUE
    )

test_that("function returns full df when db is valid", {
  testthat::expect_equal(
    nrow(clean_data(
      input_df = test_df,
      col_name_start = "organisation",
      col_name_end = "stonefly_plecoptera",
      sample_site = "sampling_site",
      locations_name = "riverfly_locs",
      data_type_name = "Urban Riverfly"
    )),
    nrow(deduped_df)
  )
})

test_that("function returns no error when db is valid", {
  testthat::expect_no_error(
    clean_data(
      input_df = test_df,
      col_name_start = "organisation",
      col_name_end = "stonefly_plecoptera",
      sample_site = "sampling_site",
      locations_name = "riverfly_locs",
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
    locations_name = "riverfly_locs",
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
    locations_name = "riverfly_locs",
    data_type_name = "Urban Riverfly"
  ))
})

test_that("function catches an entry with a duplicate site and timestamp", {
  dupe_df <- test_df
  fake_row <- dupe_df[1, ]
  dupe_df <- rbind(dupe_df, fake_row)

  testthat::expect_warning(clean_data(
    input_df = dupe_df,
    col_name_start = "organisation",
    col_name_end = "stonefly_plecoptera",
    sample_site = "sampling_site",
    locations_name = "riverfly_locs",
    data_type = "Urban Riverfly"
  ))
})
