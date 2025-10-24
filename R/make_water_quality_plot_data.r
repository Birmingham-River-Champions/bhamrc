#' make_water_quality_plot_data
#'
#' Function to create the data for the Water Quality plots
#' @param water_quality_data The cleaned water quality data
#' @param sampling_locs The sampling locations for water quality data
#' @param reading_type The type of water quality reading (e.g., "Temperature (°C)")
#' @return A data frame for the water quality plot
#' @importFrom dplyr left_join select mutate across
#' @importFrom RColorBrewer brewer.pal
make_water_quality_plot_data <- function(
    water_quality_data,
    sampling_locs,
    plot_palette
) {
    browser()
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
        mutate(
            ####Need to better format these first three in terms of ranges, nice colours etc
            WQ_Plot_Colour = case_when(
                reading_type == "conductivity_mS" & value <= 350 ~
                    plot_palette[1],
                reading_type == "conductivity_mS" & value > 350 & value < 450 ~
                    plot_palette[2],
                reading_type == "conductivity_mS" & value >= 450 & value < 550 ~
                    plot_palette[3],
                reading_type == "conductivity_mS" & value >= 550 & value < 650 ~
                    plot_palette[4],
                reading_type == "conductivity_mS" & value >= 650 ~
                    plot_palette[5],
                reading_type == "temperature_C" & value < 5 ~ plot_palette[1],
                reading_type == "temperature_C" & value >= 5 & value < 10 ~
                    plot_palette[2],
                reading_type == "temperature_C" & value >= 10 & value < 20 ~
                    plot_palette[3],
                reading_type == "temperature_C" & value >= 20 & value < 25 ~
                    plot_palette[4],
                reading_type == "temperature_C" & value >= 25 ~ plot_palette[5],
                reading_type == "ammonia_ppm" & value < 0.05 ~ plot_palette[1],
                reading_type == "ammonia_ppm" & value >= 0.05 & value < 0.15 ~
                    plot_palette[2],
                reading_type == "ammonia_ppm" & value >= 0.15 & value < .25 ~
                    plot_palette[3],
                reading_type == "ammonia_ppm" & value >= 0.25 & value < .58 ~
                    plot_palette[4],
                reading_type == "ammonia_ppm" & value >= 0.58 ~ plot_palette[5],
                reading_type == "phosphate_ppm" & value <= 0.05 ~
                    plot_palette[1],
                reading_type == "phosphate_ppm" &
                    (value >= 0.05 & value < 0.1) ~
                    plot_palette[2],
                reading_type == "phosphate_ppm" & (value >= 0.2 & value < 0.5) ~
                    plot_palette[3],
                reading_type == "phosphate_ppm" & (value >= 0.5 & value <= 1) ~
                    plot_palette[4],
                reading_type == "phosphate_ppm" & value > 1 ~ plot_palette[5],
                reading_type == "nitrate_ppm" & value <= 0.5 ~ plot_palette[1],
                reading_type == "nitrate_ppm" & (value > 0.5 & value <= 1) ~
                    plot_palette[2],
                reading_type == "nitrate_ppm" & value > 1 & value <= 2 ~
                    plot_palette[3],
                reading_type == "nitrate_ppm" & (value > 2 & value <= 5) ~
                    plot_palette[4],
                reading_type == "nitrate_ppm" & value > 5 ~ plot_palette[5],
                reading_type == "turbidity_NTU" & value < 17 ~ plot_palette[1],
                reading_type == "turbidity_NTU" & value >= 17 & value < 25 ~
                    plot_palette[2],
                reading_type == "turbidity_NTU" & value >= 25 & value < 40 ~
                    plot_palette[3],
                reading_type == "turbidity_NTU" & value >= 40 & value < 100 ~
                    plot_palette[4],
                reading_type == "turbidity_NTU" & value >= 100 ~ plot_palette[5]
            )
        ) |>
        dplyr::mutate(survey_date = dmy(survey_date))

    return(water_quality_plots)
}
