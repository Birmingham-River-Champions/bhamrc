#' species_plots
#'
#' Function to create the data for the Riverfly plots
#' @param riverfly_data The cleaned Riverfly data
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
            "sampling_site",
            "LAT",
            "LONG"
        )],
        by = join_by(sampling_site),
        multiple = "first"
    ) |>
        dplyr::select(organisation, everything())

    # Anonymise those that want to be based on the sign up sheet
    # flip names, and remove parenthesised orgs
    Riverfly_Species_Plot <- Riverfly_Species_Plot |>
        anonymise_organisations() |>
        remove_parenthesised_orgs() |>
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
