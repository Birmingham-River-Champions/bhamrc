test_that("ARMI calculation returns what you expect", {
  #Manual riverfly calculation
  test_df <- test_fixture_riverfly()[[1]]

  # observations of cased caddisflies get a value of ARMI = 2 between 10 and 99
  # ARMI scoring based on Table 5.11 (FWR Handbook)
  # Scores differ by taxon and abundance band

  # Define ARMI scoring table from Table 5.11
  armi_scores <- list(
    cased_caddisfly = c("1-9" = 1, "10-99" = 2, "100-999" = 3, ">1000" = 4),
    caseless_caddisfly = c("1-9" = 1, "10-99" = 2, "100-999" = 3, ">1000" = 4),
    olive_mayfly = c("1-9" = 1, "10-99" = 2, "100-999" = 3, ">1000" = 4),
    blue_winged_olive_mayfly = c(
      "1-9" = 1,
      "10-99" = 2,
      "100-999" = 3,
      ">1000" = 4
    ),
    freshwater_shrimp = c("1-9" = 1, "10-99" = 2, "100-999" = 3, ">1000" = 2),
    freshwater_hoglouse = c(
      "1-9" = 1,
      "10-99" = 1,
      "100-999" = 0,
      ">1000" = -2
    ),
    blackfly_larvae = c("1-9" = 1, "10-99" = 2, "100-999" = 2, ">1000" = 0),
    freshwater_worm = c("1-9" = 1, "10-99" = 1, "100-999" = 0, ">1000" = -3),
    stonefly_plecoptera = c("1-9" = 1, "10-99" = 2, "100-999" = 3, ">1000" = 4),
    freshwater_leech = c("1-9" = 1, "10-99" = 1, "100-999" = 0, ">1000" = -2),
    freshwater_snail = c("1-9" = 1, "10-99" = 1, "100-999" = 1, ">1000" = 0),
    freshwater_beetle = c("1-9" = 1, "10-99" = 1, "100-999" = 2, ">1000" = 3),
    green_drake_mayfly = c("1-9" = 1, "10-99" = 2, "100-999" = 3, ">1000" = 4),
    flat_bodied_stone_clinger_mayfly = c(
      "1-9" = 1,
      "10-99" = 2,
      "100-999" = 3,
      ">1000" = 4
    )
  )

  # Apply ARMI scores to test_df
  for (taxon in names(armi_scores)) {
    for (band in names(armi_scores[[taxon]])) {
      if (length(test_df[(test_df[[taxon]] == band), taxon]) != 0) {
        test_df[(test_df[[taxon]] == band), taxon] <- as.numeric(armi_scores[[
          taxon
        ]][band])
      }
    }
  }

  test_df[, names(riverfly_spp_bw)] <- apply(
    test_df[, names(riverfly_spp_bw)],
    2,
    as.numeric
  )
  armi_test <- test_df |>
    mutate("ARMI" = sum(!!(as.name(names(riverfly_spp_bw))), na.rm = TRUE)) |>
    select(-!!(as.name(names(riverfly_spp_bw))))

  # Use function to calculate ARMI
  test_output <- make_riverfly_ARMI("riverflytest")

  expect_equal(test_output, armi_test)
})
