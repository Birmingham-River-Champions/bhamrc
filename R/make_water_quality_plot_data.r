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
    sampling_locs,
    plot_palette
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
            one_of(unlist(water_quality_bw))
        ) |>
        tidyr::pivot_longer(
            cols = -c(organisation, survey_date, sampling_site, LONG, LAT)
        ) |>
        dplyr::rename(reading_type = "name", value = "value") |>
        mutate(value = as.numeric(value)) |>
        dplyr::mutate(survey_date = dmy(survey_date))

    # Function to add colours based on reading type and breaks
    # Still not the most efficient way to do this
    add_colours <- function(plot_data_object) {
        bin_breaks <- plot_breaks |>
            filter(metric == reading_type) |>
            select(bin_breaks) |>
            unlist()

        obj_return <- plot_data_object |>
            mutate(
                # Need to better format these first three in terms of ranges, nice colours etc
                WQ_Plot_Colour = case_when(
                    value <= bin_breaks[1] ~
                        plot_palette[1],
                    (value > bin_breaks[1]) &
                        (value <= bin_breaks[2]) ~
                        plot_palette[2],
                    (value > bin_breaks[2]) &
                        (value <= bin_breaks[3]) ~
                        plot_palette[3],
                    (value > bin_breaks[3]) &
                        (value <= bin_breaks[4]) ~
                        plot_palette[4],
                    (value > bin_breaks[4]) ~
                        plot_palette[5],
                )
            )
    }

    # Now get average value for plotting purposes - in time I want to only select the last 12 months
    Riverfly_WQ_Plot_SiteAv <- water_quality_plots |>
        select(sampling_site, reading_type, value, organisation) |>
        group_by(sampling_site, organisation, reading_type) |>
        summarise_all(mean) |>
        add_colours() |>
        ungroup()

    Riverfly_WQ_Plot_SiteAv <- Riverfly_WQ_Plot_SiteAv |>
        left_join(
            Riverfly_WQ_Plot_SiteAv,
            unique(water_quality_plots[, c("sampling_site", "LAT", "LONG")]),
            by = join_by(sampling_site),
            multiple = "first"
        )

    # Make colours for the non-averaged data frame
    water_quality_plots <- add_colours(water_quality_plots)

    return(list(
        "all_obs" = water_quality_plots,
        "recent" = Riverfly_WQ_Plot_SiteAv
    ))
}
