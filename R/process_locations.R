#' process_locations
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr bind_cols rename rename_with select contains
process_locations <- function(
    sampling_locs_url = 'https://docs.google.com/spreadsheets/d/1ZEkLC3HBkB8SJynA3pHtmntMOiCT8p4e2BFNYsMUR4c/edit?usp=sharing',
    outfall_locs_url = 'https://docs.google.com/spreadsheets/d/1JJ8bPWppVKbmCfllIevrVmt_dcoswOim7Cos418Ot6w/edit?gid=0#gid=0'
) {
    #Then bring in the locations of all of the known eco/WQ sampling points - BRC project team can be used for any site
    BRC_Sampling_Locs_raw <- as.data.frame(gsheet::gsheet2tbl(
        sampling_locs_url
    ))
    #And the same for Urban Outfall Safari locations - BRC project team can be used for any site
    BRC_Outfall_Locs_raw <- as.data.frame(gsheet::gsheet2tbl(outfall_locs_url))
    ##Was going to do invasive species separate with what3words, but code was a nightmare. Use BRC sampling sites, and in future
    ##convert what3words manually on grid reference finder and then label this as something like "Sighting 'out and about'"

    ####Now convert the site IDs into geospatial form (converting east/northing to lat/long)
    BRC_Sampling_Locs_sf <- sf::st_as_sf(
        BRC_Sampling_Locs_raw,
        coords = c("Easting", "Northing"),
        crs = 27700
    ) |>
        sf::st_transform(crs = 4326) |>
        sf::st_coordinates()

    BRC_Sampling_Locs <- BRC_Sampling_Locs_raw |>
        dplyr::bind_cols(data.frame(BRC_Sampling_Locs_sf)[, 2]) |>
        dplyr::bind_cols(data.frame(BRC_Sampling_Locs_sf)[, 1]) |>
        dplyr::rename(
            LAT = ...5,
            LONG = ...6
        ) |>
        dplyr::rename_with(
            ~ return("sampling_site"),
            contains(c("BRC.sampling.site.ID", "BRC sampling site ID"))
        )

    ###Same for Outfall locs
    BRC_Outfall_Locs_sf <-
        sf::st_as_sf(
            BRC_Outfall_Locs_raw,
            coords = c("Easting", "Northing"),
            crs = 27700
        ) |>
        sf::st_transform(crs = 4326) |>
        sf::st_coordinates()

    BRC_Outfall_Locs <- BRC_Outfall_Locs_raw |>
        dplyr::bind_cols(data.frame(BRC_Outfall_Locs_sf)[, 2]) |>
        dplyr::bind_cols(data.frame(BRC_Outfall_Locs_sf)[, 1]) |>
        dplyr::rename(LAT = ...5, LONG = ...6) |>
        dplyr::rename_with(
            ~ return("sampling_site"),
            contains(c("Outfall.ID", "Outfall ID"))
        )

    db_create("riverfly_locs")
    db_create("outfall_locs")
    # Populate the database tables with the cleaned data
    populate_db(BRC_Sampling_Locs, "riverfly_locs")
    populate_db(BRC_Outfall_Locs, "outfall_locs")

    return(list(BRC = BRC_Sampling_Locs, Outfall = BRC_Outfall_Locs))
}


# Helper function to create acceptable location identifiers
acceptable_locs <- function(df) {
    df |>
        dplyr::mutate(identifiers = paste(Organisation, sampling_site)) |>
        dplyr::select(identifiers)
}
