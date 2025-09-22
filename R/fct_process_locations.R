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
    BRC_Sampling_Locs <- as.data.frame(gsheet::gsheet2tbl(sampling_locs_url))
    #And the same for Urban Outfall Safari locations - BRC project team can be used for any site
    BRC_Outfall_Locs <- as.data.frame(gsheet::gsheet2tbl(outfall_locs_url))
    ##Was going to do invasive species separate with what3words, but code was a nightmare. Use BRC sampling sites, and in future
    ##convert what3words manually on grid reference finder and then label this as something like "Sighting 'out and about'"

    ####Now convert the site IDs into geospatial form (converting east/northing to lat/long)
    BRC_Sampling_Locs_sf <- sf::st_as_sf(
        BRC_Sampling_Locs,
        coords = c("Easting", "Northing"),
        crs = 27700
    ) |>
        sf::st_transform(crs = 4326)
    BRC_Sampling_Locs$LAT <-
        data.frame(sf::st_coordinates(BRC_Sampling_Locs_sf))[, 2]
    BRC_Sampling_Locs$LONG <-
        data.frame(sf::st_coordinates(BRC_Sampling_Locs_sf))[, 1]

    ##Now just need to bring in the sampling locations - first keep one organisation per SITE ID (i.e., remove BRC project partners except only those I'm sampling)
    Unique_BRC_Sampling_Locs <- BRC_Sampling_Locs |>
        dplyr::distinct(`BRC.sampling.site.ID`, .keep_all = TRUE) |>
        dplyr::rename(ID = BRC.sampling.site.ID)

    ###Same for Outfall locs
    BRC_Outfall_Locs_sf <-
        sf::st_as_sf(
            BRC_Outfall_Locs,
            coords = c("Easting", "Northing"),
            crs = 27700
        ) |>
        sf::st_transform(crs = 4326)
    BRC_Outfall_Locs$LAT <-
        data.frame(sf::st_coordinates(BRC_Outfall_Locs_sf))[, 2]
    BRC_Outfall_Locs$LONG <-
        data.frame(sf::st_coordinates(BRC_Outfall_Locs_sf))[, 1]
    ##Now just need to bring in the sampling locations - first keep one organisation per SITE ID (i.e., remove BRC project partners except only those I'm sampling)
    Unique_Outfall_Locs <- BRC_Outfall_Locs |>
        dplyr::distinct(`Outfall.ID`, .keep_all = TRUE) |>
        dplyr::rename(ID = Outfall.ID)

    write.csv(
        Unique_BRC_Sampling_Locs,
        "./inst/extdata/BRC_Sampling_Locs.csv",
        row.names = FALSE
    )

    write.csv(
        Unique_Outfall_Locs,
        "./inst/extdata/BRC_Outfall_Locs.csv",
        row.names = FALSE
    )
    return(list(BRC = Unique_BRC_Sampling_Locs, Outfall = Unique_Outfall_Locs))
}
