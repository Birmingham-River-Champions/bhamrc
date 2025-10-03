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

species_plots <- function() {
    Riverfly_Species_Plot <- left_join(
        Riverfly_data,
        Unique_BRC_Sampling_Locs,
        by = c("BRC sampling site ID...5" = "BRC sampling site ID")
    ) %>%
        dplyr::select(-c(Easting, Northing)) %>%
        dplyr::select(Organisation, everything())
    ###Anonymise those that want to be based on the sign up sheet
    Riverfly_Species_Plot$Organisation <- ifelse(
        Riverfly_Species_Plot$Organisation == "Friends of Lifford Reservoir",
        "Anonymous",
        Riverfly_Species_Plot$Organisation
    )

    #######Rename site ID, remove the parenthsised organisation from the site ID and flip
    Riverfly_Species_Plot <- Riverfly_Species_Plot %>%
        dplyr::rename(`BRC site ID` = "BRC sampling site ID...5")
    Riverfly_Species_Plot$`BRC site ID` <- gsub(
        "\\s*\\(.*\\)$",
        "",
        Riverfly_Species_Plot$`BRC site ID`
    )
    Riverfly_Species_Plot <- Riverfly_Species_Plot %>%
        mutate(across(c("BRC site ID"), flip_site_names))

    #########Create 2 separate sets - One of other species and a table (like invasive species) - do this first for now
    Riverfly_Other_Species_Plot <- Riverfly_Species_Plot %>%
        dplyr::select(-contains("Number of"))
    ########Then the other set for Urban Riverfly taxa and a ggplot (like ARMI) - did this second here so I can overwrite df name
    Riverfly_Species_Plot <- Riverfly_Species_Plot %>%
        dplyr::select(-contains("Additional taxa"))

    # Identify the columns that match the pattern "^Number of"
    # Replace the column names for those that match the pattern
    names(Riverfly_Species_Plot)[grepl(
        "^Number of",
        names(Riverfly_Species_Plot)
    )] <-
        c(
            "Cased caddisfly (Trichoptera)",
            "Caseless caddisfly (Trichoptera)",
            "Olive mayfly (Baetidae)",
            "Blue-winged olive mayfly (Ephemerellidae)",
            "Freshwater shrimp (Gammaridae)",
            "Freshwater hoglouse (Asellidae)",
            "Blackfly larvae (Simuliidae)",
            "Freshwater worm (Oligochaeta)",
            "Freshwater leech (Hirudinea)",
            "Freshwater snail (Gastropoda)",
            "Freshwater beetle (Coleoptera)",
            "Green drake mayfly (Ephemeridae)",
            "Flat-bodied stone clinger mayfly (Heptageniidae)",
            "Stonefly larvae (Plecoptera)"
        )

    ###Melt - good to go for ggplot, converted to numeric as it represents y-axis (labels edited in ggplot code in server below)
    Riverfly_Species_Plot <- Riverfly_Species_Plot %>%
        melt(
            .,
            id = c("Organisation", "BRC site ID", "Survey date", "LONG", "LAT")
        ) %>%
        dplyr::rename(Taxa = "variable", Abundance = "value") %>%
        group_by(`BRC site ID`, Taxa) %>%
        filter(!all(Abundance == "0")) %>%
        ungroup() %>%
        mutate(
            Abundance = as.numeric(case_when(
                Abundance == "0" ~ 0,
                Abundance == "1-9" ~ 1,
                Abundance == "10-99" ~ 2,
                Abundance == "100-999" ~ 3,
                Abundance == ">1000" ~ 4
            ))
        ) %>%
        mutate(`Survey date` = dmy(`Survey date`))

    ###Then find the highest/most recent - factor fine for the purpose of the leaflet marker
    Riverfly_Species_Plot_Recent <- Riverfly_Species_Plot %>%
        filter(`Survey date` >= Sys.Date() - years(3)) %>%
        arrange(`BRC site ID`, Taxa, desc(Abundance), desc(`Survey date`)) %>%
        group_by(`BRC site ID`, Taxa) %>%
        slice_head(n = 1) %>%
        ungroup() %>%
        mutate(
            Riverfly_Species_Colour = factor(
                case_when(
                    Abundance == 0 ~ brewer.pal(n = 5, name = "Greys")[1],
                    Abundance == 1 ~ brewer.pal(n = 5, name = "Greys")[2],
                    Abundance == 2 ~ brewer.pal(n = 5, name = "Greys")[3],
                    Abundance == 3 ~ brewer.pal(n = 5, name = "Greys")[4],
                    Abundance == 4 ~ brewer.pal(n = 5, name = "Greys")[5]
                ),
                levels = brewer.pal(n = 5, name = "Greys")
            )
        )
    ################Now go back to the 'other species'############################
    names(Riverfly_Other_Species_Plot)[grepl(
        "^Additional taxa",
        names(Riverfly_Other_Species_Plot)
    )] <-
        c(
            "Non-biting midge larvae (Chironomidae)",
            "Cranefly larvae (Dicranota sp.)",
            "Other cranefly larvae (Tipulidae)",
            "Water mite (Hydracarina)",
            "Net spinning (caseless) caddisfly (Hydropsychidae)",
            "Green sedge (caseless) caddisfly (Rhyacophilidae)",
            "Ramshorn snail (Planorbidae)",
            "Freshwater mollusc (Sphaeriidae)",
            "Freshwater limpet (Acroloxidae/Ancylidae)",
            "Bullhead (fish - Cottus gobio)"
        )

    ##Organise - this plot df not used in the end. Seemed off trying to plot inconsistently ID'd taxa. So did a pop up table of the most recent instead (like invasive)
    Riverfly_Other_Species_Plot <- Riverfly_Other_Species_Plot %>%
        melt(
            .,
            id = c("Organisation", "BRC site ID", "Survey date", "LONG", "LAT")
        ) %>%
        dplyr::rename(Taxa = "variable", Abundance = "value") %>%
        group_by(`BRC site ID`, Taxa) %>%
        filter(!all(Abundance == "0")) %>%
        ungroup() %>%
        mutate(
            Abundance = factor(
                Abundance,
                levels = c(">1000", "100-999", "10-99", "1-9")
            )
        ) %>%
        mutate(`Survey date` = dmy(`Survey date`))

    ###Then find the highest/most recent
    Riverfly_Other_Species_Plot_Recent <- Riverfly_Other_Species_Plot %>%
        filter(`Survey date` >= Sys.Date() - years(3)) %>%
        arrange(`BRC site ID`, Taxa, Abundance, desc(`Survey date`)) %>%
        group_by(`BRC site ID`, Taxa) %>%
        slice_head(n = 1) %>%
        ungroup()
}
