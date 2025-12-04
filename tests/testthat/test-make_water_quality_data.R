test_that("make_water_quality_data works", {
    test_data <- test_fixture_riverfly()$water_quality
    test_locs <- test_fixture_riverfly()$riverfly_locs

    plot_test_data <- left_join(
        test_data,
        test_locs[, c(
            "sampling_site",
            "LAT",
            "LONG"
        )],
        by = join_by(sampling_site),
        multiple = "first"
    ) |>
        dplyr::mutate(ammonia_ppm = as.numeric(ammonia_ppm))

    water_quality_test <- select(
        plot_test_data,
        organisation,
        survey_date,
        LONG,
        LAT,
        sampling_site,
        one_of(names(water_quality_bw)),
    ) |>
        pivot_longer(
            -c(
                organisation,
                survey_date,
                sampling_site,
                LONG,
                LAT
            )
        ) |>
        rename(reading_type = "name") |>
        mutate(survey_date = dmy(survey_date)) |>
        remove_parenthesised_orgs() |>
        mutate(across(sampling_site, flip_site_names)) |>
        anonymise_organisations()

    recent_avg_wq <- water_quality_test |>
        group_by(sampling_site, reading_type) |>
        filter(
            survey_date >= max(survey_date) - 365 &
                survey_date <= max(survey_date)
        ) |>
        summarise(
            value = mean(as.numeric(value), na.rm = TRUE),
            LAT = dplyr::first(LAT),
            LONG = dplyr::first(LONG),
            organisation = dplyr::first(organisation)
        ) |>
        ungroup()

    water_quality_test_full <- water_quality_test

    actual_water_quality_plot_data <- make_water_quality_plot_data(
        water_quality_data = test_data,
        sampling_locs = test_locs
    )

    expect_equal(
        water_quality_test_full,
        actual_water_quality_plot_data$all_obs
    )
})
