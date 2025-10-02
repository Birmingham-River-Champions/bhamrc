#' process_locations
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
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
        dplyr::rename(ID = BRC.sampling.site.ID, LAT = ...5, LONG = ...6)

    ##Now just need to bring in the sampling locations - first keep one organisation per SITE ID (i.e., remove BRC project partners except only those I'm sampling)
    Unique_BRC_Sampling_Locs <- BRC_Sampling_Locs |>
        dplyr::distinct(ID, .keep_all = TRUE)

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
        dplyr::rename(ID = Outfall.ID, LAT = ...5, LONG = ...6)

    ##Now just need to bring in the sampling locations - first keep one organisation per SITE ID (i.e., remove BRC project partners except only those I'm sampling)
    Unique_Outfall_Locs <- BRC_Outfall_Locs |>
        dplyr::distinct(ID, .keep_all = TRUE)

    write.csv(
        Unique_BRC_Sampling_Locs,
        "./inst/extdata/Unique_BRC_Sampling_Locs.csv",
        row.names = FALSE
    )

    write.csv(
        Unique_Outfall_Locs,
        "./inst/extdata/Unique_BRC_Outfall_Locs.csv",
        row.names = FALSE
    )
    return(list(BRC = BRC_Sampling_Locs, Outfall = BRC_Outfall_Locs))
}


# Helper function to create acceptable location identifiers
acceptable_locs <- function(df) {
    df |>
        dplyr::mutate(identifiers = paste(Organisation, ID)) |>
        dplyr::select(identifiers)
}
