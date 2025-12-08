load_data <- function() {
    # Pull the processed data from the db for riverfly, invasive species, and water quality
    con <- DBI::dbConnect(
        RSQLite::SQLite(),
        "data.sqlite",
        extended_types = TRUE
    )
    riverfly_data <- DBI::dbReadTable(con, "riverfly")
    BRCInvSpcs <- DBI::dbReadTable(con, "invasive_species")
    BRC_locs <- DBI::dbReadTable(con, "riverfly_locs")
    BRC_wq <- DBI::dbReadTable(con, "water_quality")
    dbDisconnect(con)

    # Generate the modified plot data for the riverfly plots
    Unique_BRC_Sampling_Locs <- BRC_locs |>
        dplyr::distinct(sampling_site, .keep_all = TRUE)

    Riverfly_Species_Plot_All <- species_plots(
        riverfly_data,
        Unique_BRC_Sampling_Locs
    )

    # If the user chooses ARMI, calculate the ARMI scores and plot data
    ARMI_assignment <- make_riverfly_ARMI(select(riverfly_data, -c(id)))
    ARMI_data <- sum_up_ARMI(ARMI_assignment)
    riverflyARMIDataList <- make_ARMI_plot_data(
        ARMI_data,
        Unique_BRC_Sampling_Locs
    )

    # If the user chooses Water Chemistry, plot water quality data
    WQ_plot_data <- make_water_quality_plot_data(
        BRC_wq,
        Unique_BRC_Sampling_Locs
    )

    return(list(
        riverfly_data = riverfly_data,
        BRCInvSpcs = BRCInvSpcs,
        BRC_locs = BRC_locs,
        BRC_wq = BRC_wq,
        Riverfly_Species_Plot = Riverfly_Species_Plot_All[[1]],
        Riverfly_Species_Plot_Recent = Riverfly_Species_Plot_All[[2]],
        Riverfly_Other_Species_Plot = Riverfly_Species_Plot_All[[3]],
        Riverfly_Other_Species_Plot_Recent = Riverfly_Species_Plot_All[[4]],
        riverflyARMISiteAv = riverflyARMIDataList[[2]],
        Riverfly_ARMI_Plot = riverflyARMIDataList[[1]],
        Unique_BRC_Sampling_Locs = Unique_BRC_Sampling_Locs,
        WQ_plot_data = WQ_plot_data
    ))
}
