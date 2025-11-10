#' make_ARMI_plot_data
#'
#' @description A function to make the plot object for the ARMI map and tooltip graphs
#' @param Riverfly_ARMI_Calc A dataframe output from make_riverfly_ARMI that contains
#' riverfly data and the ARMI column
#' @param Unique_BRC_Sampling_Locs a data frame that contains the geographic coordinates
#' for the sampling sites.
#' @return averaged site ARMI calculations to plot on the riverfly figure
#' @importFrom dplyr left_join mutate summarise_all
make_ARMI_plot_data <- function(Riverfly_ARMI_Calc, Unique_BRC_Sampling_Locs) {
    ##Now bring this in
    Riverfly_ARMI_Plot <-
        left_join(
            Riverfly_ARMI_Calc,
            Unique_BRC_Sampling_Locs,
            by = join_by(sampling_site)
        )

    # Get the breaks for the ARMI scoring
    breaks_vector <- unlist(
        filter(plot_breaks, reading_type == "ARMI")$bin_breaks
    )

    ##And colour code the point according to the ARMI score
    Riverfly_ARMI_Plot <- Riverfly_ARMI_Plot |>
        mutate(
            ARMI_Plot_Colour = cut(
                ARMI,
                breaks = c(-Inf, breaks_vector, Inf),
                labels = c(brewer.pal(n = 5, name = "Blues"))
            )
        ) |>
        dplyr::select(organisation, everything())

    # Remove the parenthsised organisation from the site ID
    Riverfly_ARMI_Plot <- Riverfly_ARMI_Plot |>
        remove_parenthesised_orgs()
    # Now also flip the names around so the RIVER comes after the Site

    # Code was from ChatGPT initially designed for multiple columns, but works fine.
    Riverfly_ARMI_Plot <- Riverfly_ARMI_Plot |>
        mutate(across(c(sampling_site), flip_site_names)) |>
        dplyr::select(-c(Easting, Northing)) |>
        mutate(survey_date = dmy(survey_date)) |>
        arrange(sampling_site, survey_date) |>
        anonymise_organisations()

    # Now get average value for plotting purposes - in time I want to only select the last 12 months
    Riverfly_ARMI_Plot_SiteAv <- Riverfly_ARMI_Plot |>
        select(sampling_site, ARMI, organisation) |>
        group_by(sampling_site, organisation) |>
        summarise_all(mean) |>
        mutate(
            ARMI_Plot_Colour = cut(
                ARMI,
                breaks = c(-Inf, breaks_vector, Inf),
                labels = c(brewer.pal(n = 5, name = "Blues"))
            )
        ) |>
        ungroup()

    Riverfly_ARMI_Plot_SiteAv <- left_join(
        Riverfly_ARMI_Plot_SiteAv,
        unique(Riverfly_ARMI_Plot[, c("sampling_site", "LAT", "LONG")]),
        multiple = "first"
    )
    return(list(Riverfly_ARMI_Plot, Riverfly_ARMI_Plot_SiteAv))
}
