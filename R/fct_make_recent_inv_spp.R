#' make_recent_inv_spp
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr left_join select rename arrange filter mutate across group_by slice_head ungroup
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom RColorBrewer brewer.pal
#' @importFrom lubridate dmy
#' @noRd
make_recent_inv_spp <- function(cleaned_data, sampling_locs, plot_palette) {
    BRCInvSpcs_Plot <- ##Needed to make a dataframe and not a tibble for melt to work
        dplyr::left_join(
            # Select lat and long from the locations data frame and add to the invasive species date
            cleaned_data, # only the first lat and long are used per location
            sampling_locs[, c(
                "sampling_site",
                "LAT",
                "LONG"
            )],
            multiple = "first"
        ) |>
        dplyr::select(
            -invasive_spp_what_three_words,
            -any_other_invasive_spp
        )
    BRCInvSpcs_Plot <- BRCInvSpcs_Plot |> # Remove unneeded columns
        tidyr::pivot_longer(
            -c(
                organisation,
                survey_date,
                invasive_spp_sampling_date,
                sampling_site,
                LONG,
                LAT
            ),
            names_to = "variable",
            values_to = "value"
        ) |>
        dplyr::filter(value != "") |>
        dplyr::rename(c(
            'measurement' = variable,
            'abundance' = value
        )) |>
        mutate(
            InvSpcs_Plot_Colour = dplyr::case_when(
                abundance == ">1000" ~ plot_palette[1],
                abundance == "100-999" ~ plot_palette[2],
                abundance == "10-99" ~ plot_palette[3],
                abundance == "1-9" ~ plot_palette[4],
                abundance == "Abundant (>33%)" ~ plot_palette[1],
                abundance == "Present (1-33%)" ~ plot_palette[3]
            )
        ) |>
        mutate(date_time = lubridate::dmy(invasive_spp_sampling_date)) |>
        dplyr::arrange(sampling_site, date_time) |>
        dplyr::select(-survey_date, -invasive_spp_sampling_date) |> ##Arrange by date
        dplyr::mutate(sampling_site = gsub("\\s+", " ", sampling_site)) |> ###################Remove the parenthsised organisation from the site ID
        mutate(across(sampling_site, flip_site_names))

    ##Anonymise those that want to be based on the sign up sheet
    BRCInvSpcs_Plot$organisation <- ifelse(
        BRCInvSpcs_Plot$organisation == "Friends of Lifford Reservoir",
        "Anonymous",
        BRCInvSpcs_Plot$organisation
    )

    BRCInvSpcs_Plot_Recent <- BRCInvSpcs_Plot |>
        filter(date_time >= Sys.Date() - lubridate::years(3))
    BRCInvSpcs_Plot_Recent$abundance <- factor(
        BRCInvSpcs_Plot_Recent$abundance,
        levels = c(
            ">1000",
            "100-999",
            "10-99",
            "1-9",
            "Abundant (>33%)",
            "Present (1-33%)"
        )
    )

    # This selects the highest observation per taxa per site based on the factor levels set above
    BRCInvSpcs_Plot_Recent <- BRCInvSpcs_Plot_Recent |>
        dplyr::arrange(
            sampling_site,
            measurement,
            abundance,
            desc(date_time)
        ) |>
        dplyr::group_by(sampling_site, measurement) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()

    return(BRCInvSpcs_Plot_Recent)
}

flip_site_names <- function(site_name) {
    # Use regex to capture the two parts of the string
    gsub("^(\\w+(?: \\w+)*),\\s*(.*)$", "\\2, \\1", site_name)
}
