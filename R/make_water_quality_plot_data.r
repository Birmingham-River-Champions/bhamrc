#' make_water_quality_plot_data
#'
#' Function to create the data for the Water Quality plots
#' @param water_quality_data The cleaned water quality data
#' @param sampling_locs The sampling locations for water quality data
#' @param reading_type The type of water quality reading (e.g., "Temperature (\u00B0C)")
#' @return A data frame for the water quality plot
#' @importFrom dplyr left_join select mutate across
#' @importFrom RColorBrewer brewer.pal
make_water_quality_plot_data <- function(
    water_quality_data,
    sampling_locs
) {
    # Get locations of water quality observations
    water_quality_plots <- left_join(
        water_quality_data,
        sampling_locs[, c(
            "sampling_site",
            "LAT",
            "LONG"
        )],
        by = join_by(sampling_site),
        multiple = "first"
    ) |>
        dplyr::select(organisation, everything()) |>
        anonymise_organisations() |>
        remove_parenthesised_orgs() |>
        mutate(across(sampling_site, flip_site_names)) |>
        mutate(ammonia_ppm = as.numeric(ammonia_ppm))

    # Make the data into long format and cast into correct types
    water_quality_plots <- water_quality_plots |>
        dplyr::select(
            organisation,
            survey_date,
            LONG,
            LAT,
            sampling_site,
            one_of(names(water_quality_bw))
        ) |>
        tidyr::pivot_longer(
            cols = -c(organisation, survey_date, sampling_site, LONG, LAT)
        ) |>
        dplyr::rename(reading_type = "name", value = "value") |>
        mutate(value = as.numeric(value)) |>
        dplyr::mutate(survey_date = dmy(survey_date)) |>
        drop_na()

    # Now get average value for plotting purposes - in time I want to only select the last 12 months
    site_average <- water_quality_plots |>
        filter(survey_date >= Sys.Date() - years(3)) |>
        group_by(sampling_site, organisation, reading_type) |>
        summarise(value = mean(value)) |>
        ungroup()

    WQ_Plot_SiteAv <- site_average |>
        left_join(
            unique(water_quality_plots[, c("sampling_site", "LAT", "LONG")]),
            by = join_by(sampling_site),
            multiple = "first"
        )

    # Split the data frames by reading type for easier plotting later
    water_quality_plots <- split(
        water_quality_plots,
        f = list(
            water_quality_plots$reading_type,
            water_quality_plots$sampling_site
        )
    )
    # Split the averaged data frames by reading type for easier plotting later
    WQ_Plot_SiteAv <- split(
        WQ_Plot_SiteAv,
        f = WQ_Plot_SiteAv$reading_type
    )

    return(list(
        "all_obs" = water_quality_plots,
        "recent" = WQ_Plot_SiteAv
    ))
}
