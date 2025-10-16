test_df <- test_fixture_riverfly()[[1]]
locations_name <- "BRC_Sampling_Locs"

test_that("function returns full df when db is valid", {
  # Create a temporary database and populate it with the test data
  names(test_df)[5] <- "sampling_site_riverfly"
  test_df$data_type <- "Urban Riverfly"

  db_create_and_pop(
    full_form = test_df,
    locations_list = "riverfly_locs",
    data_type = "Urban Riverfly",
    index_of_site_col = 4,
    table_name = "riverflytest",
    db_path = "data.sqlite"
  )

  # Connect to the temporary database and read the data back
  con <- dbConnect(
    RSQLite::SQLite(),
    testthat::test_path("../../data.sqlite"),
    extended_types = TRUE
  )
  test_riverfly <- DBI::dbReadTable(con, "riverflytest")

  on.exit(
    {
      dbRemoveTable(con, "riverflytest")
      dbDisconnect(con)
    },
    add = TRUE,
    after = FALSE
  )
  names(test_df)[5] <- "sampling_site"

  testthat::expect_equal(
    nrow(clean_data(
      input_df = test_df,
      col_name_start = "organisation",
      col_name_end = "stonefly_plecoptera",
      sample_site = "sampling_site",
      locations_name = "riverfly_locs",
      data_type_name = "Urban Riverfly"
    )),
    nrow(test_riverfly[, c(2:19)])
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
