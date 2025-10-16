#' make_recent_inv_spp
#'
#' @description This function makes the data for the recent invasive species plot
#' @param cleaned_data The cleaned invasive species data
#' @param sampling_locs The sampling locations for invasive species data
#' @param plot_palette The colour palette to use for the plot
#' @return The data frame for the recent invasive species plot.
#' @importFrom dplyr left_join select rename arrange filter mutate across group_by slice_head ungroup
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom RColorBrewer brewer.pal
#' @importFrom lubridate dmy years
#' @noRd
make_recent_inv_spp <- function(cleaned_data, sampling_locs, plot_palette) {
    BRCInvSpcs_Plot <-
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
            -id,
            -invasive_spp_what_three_words,
            -any_other_invasive_spp,
            -data_type
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
        dplyr::select(-survey_date, -invasive_spp_sampling_date) |>
        dplyr::mutate(sampling_site = gsub("\\s+", " ", sampling_site)) |> # Remove the parenthesised organisation from the site ID
        mutate(across(sampling_site, flip_site_names))

    # Anonymise those that want to be based on the sign up sheet
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

#' species_plots
#'
#' Function to create the data for the Riverfly plots
#' @param sampling_locs The sampling locations for Riverfly data
#' @return a list with four elements: Riverfly_Species_Plot which
#' has all Riverfly species, Riverfly_Species_Plot_Recent which has
#' all observations of Riverfly species for the past year,
#' Other_Species_Plot which has all observations of other species, and
#' Other_Species_Plot_Recent
#' @importFrom dplyr select rename group_by mutate ungroup case_when
#' @importFrom tidyr pivot_longer
species_plots <- function(riverfly_data, sampling_locs) {
    # Join the riverfly data with the locations data to get lat and long
    Riverfly_Species_Plot <- left_join(
        riverfly_data,
        sampling_locs[, c(
            "ID",
            "LAT",
            "LONG"
        )],
        by = c("sampling_site" = "ID"),
        multiple = "first"
    ) |>
        dplyr::select(organisation, everything())

    # Anonymise those that want to be based on the sign up sheet
    Riverfly_Species_Plot$organisation <- ifelse(
        Riverfly_Species_Plot$organisation == "Friends of Lifford Reservoir",
        "Anonymous",
        Riverfly_Species_Plot$organisation
    )
    # Rename site ID, remove the parenthsised organisation from the site ID and flip
    Riverfly_Species_Plot$sampling_site <- gsub(
        "\\s*\\(.*\\)$",
        "",
        Riverfly_Species_Plot$sampling_site
    )
    Riverfly_Species_Plot <- Riverfly_Species_Plot |>
        mutate(across(sampling_site, flip_site_names))

    # Create 2 separate sets - One of other species and a table (like invasive species) - do this first for now
    Riverfly_Other_Species_Plot <- Riverfly_Species_Plot |>
        dplyr::select(
            survey_date,
            sampling_site,
            organisation,
            LAT,
            LONG,
            one_of(names(other_spp_bw))
        )
    # Then the other set for Urban Riverfly taxa and a ggplot (like ARMI) - did this second here so I can overwrite df name
    Riverfly_Species_Plot <- Riverfly_Species_Plot |>
        dplyr::select(
            survey_date,
            sampling_site,
            organisation,
            LAT,
            LONG,
            one_of(names(riverfly_spp_bw))
        )

    # Converted to numeric as it represents y-axis (labels edited in ggplot code in server below)
    Riverfly_Species_Plot <- Riverfly_Species_Plot |>
        pivot_longer(
            -c(
                organisation,
                survey_date,
                sampling_site,
                LONG,
                LAT
            )
        ) |>
        dplyr::rename(taxa = "name", abundance = "value") |>
        group_by(sampling_site, taxa) |>
        filter(!all(abundance == "0")) |>
        ungroup() |>
        mutate(
            abundance = as.numeric(case_when(
                abundance == "0" ~ 0,
                abundance == "1-9" ~ 1,
                abundance == "10-99" ~ 2,
                abundance == "100-999" ~ 3,
                abundance == ">1000" ~ 4
            ))
        ) |>
        mutate(survey_date = dmy(survey_date))

    # Then find the highest/most recent - factor fine for the purpose of the leaflet marker
    Riverfly_Species_Plot_Recent <- Riverfly_Species_Plot |>
        filter(survey_date >= Sys.Date() - years(3)) |>
        arrange(sampling_site, taxa, desc(abundance), desc(survey_date)) |>
        group_by(sampling_site, taxa) |>
        slice_head(n = 1) |>
        ungroup() |>
        mutate(
            Riverfly_Species_Colour = factor(
                case_when(
                    abundance == 0 ~ brewer.pal(n = 5, name = "Greys")[1],
                    abundance == 1 ~ brewer.pal(n = 5, name = "Greys")[2],
                    abundance == 2 ~ brewer.pal(n = 5, name = "Greys")[3],
                    abundance == 3 ~ brewer.pal(n = 5, name = "Greys")[4],
                    abundance == 4 ~ brewer.pal(n = 5, name = "Greys")[5]
                ),
                levels = brewer.pal(n = 5, name = "Greys")
            )
        )

    # Organise - this plot df not used in the end. Seemed off trying to plot inconsistently ID'd taxa. So did a pop up table of the most recent instead (like invasive)
    Riverfly_Other_Species_Plot <- Riverfly_Other_Species_Plot |>
        pivot_longer(
            -c(organisation, sampling_site, survey_date, LONG, LAT)
        ) |>
        dplyr::rename(taxa = "name", abundance = "value") |>
        group_by(sampling_site, taxa) |>
        filter(!all(abundance == "0")) |>
        ungroup() |>
        mutate(
            abundance = factor(
                abundance,
                levels = c(">1000", "100-999", "10-99", "1-9")
            )
        ) |>
        mutate(survey_date = dmy(survey_date))

    # Then find the highest/most recent
    Riverfly_Other_Species_Plot_Recent <- Riverfly_Other_Species_Plot |>
        filter(survey_date >= Sys.Date() - years(3)) |>
        arrange(sampling_site, taxa, abundance, desc(survey_date)) |>
        group_by(sampling_site, taxa) |>
        slice_head(n = 1) |>
        ungroup()

    return(list(
        Riverfly_Species_Plot,
        Riverfly_Species_Plot_Recent,
        Riverfly_Other_Species_Plot,
        Riverfly_Other_Species_Plot_Recent
    ))
}
