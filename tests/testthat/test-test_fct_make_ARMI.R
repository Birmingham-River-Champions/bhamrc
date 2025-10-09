test_that("ARMI calculation returns what you expect", {
  #Manual riverfly calculation
  test_df <- test_fixture_riverfly()[[1]]

  # observations of cased caddisflies get a value of ARMI = 2 between 10 and 99
  test_df[(test_df$cased_caddisfly == "1-9"), "cased_caddisfly"] <- 1
  test_df[(test_df$cased_caddisfly == "10-99"), "cased_caddisfly"] <- 2
  # 1-9 cased caddisflies get an ARMI score of 1
  test_df[(test_df$cased_caddisfly == "100-999"), "cased_caddisfly"] <- 3
  # 1000+ cased caddisflies get an ARMI score of 4
  test_df[(test_df$cased_caddisfly == ">1000"), "cased_caddisfly"] <- 4

  # observations of stoneflies get a value of ARMI = 1 between 1 and 9, 2 for 10-99
  test_df[(test_df$stonefly_plecoptera == "1-9"), "stonefly_plecoptera"] <- 1
  test_df[(test_df$stonefly_plecoptera == "10-99"), "stonefly_plecoptera"] <- 2
  # 100-999 stoneflies get an ARMI score of 3
  test_df[
    (test_df$stonefly_plecoptera == "100-999"),
    "stonefly_plecoptera"
  ] <- 3
  # 1000+ stoneflies get an ARMI score of 4
  test_df[(test_df$stonefly_plecoptera == ">1000"), "stonefly_plecoptera"] <- 4

  armi_test <- test_df |>
    mutate(ARMI <- sum(cased_caddisfly, stonefly_plecoptera)) |>
    select(-cased_caddifly, -stonefly_plecoptera)

  # Use function to calculate ARMI
  test_output <- make_riverfly_ARMI("riverflytest")

  expect_equal(test_output, armi_test)
})
