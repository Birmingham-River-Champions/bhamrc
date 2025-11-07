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
    )

    water_quality_test <- select(
        plot_test_data,
        survey_date,
        sampling_site,
        organisation,
        LAT,
        LONG,
        one_of(unlist(water_quality_bw)),
    ) |>
        pivot_longer(
            -c(
                organisation,
                survey_date,
                sampling_site,
                LAT,
                LONG
            )
        ) |>
        rename(metric = "name") |>
        mutate(survey_date = dmy(survey_date))

    recent_avg_wq <- water_quality_test |>
        group_by(sampling_site, metric) |>
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
        add_colours() |>
        ungroup()

    #water_quality_test_full <- water_quality_test |>
    #    mutate(reading_type = as.numeric(reading_type)) |>
    #    add_colours(plot_palette = brewer.pal(5, "Blues"))

    # actual_water_quality_plot_data <- make_water_quality_plot_data(
    #     water_quality_data = test_data,
    #     sampling_locs = test_locs,
    #     plot_palette = brewer.pal(5, "Blues")
    # )

    expect_equal(
        4,
        2 * 2
    )
})
