#' This function adds polygons and lines to the map based on the zoom level.
#' @param mapProxy A leaflet map proxy object.
#' @param zoomLevel The current zoom level of the map.
#' @importFrom leaflet clearGroup addPolygons addPolylines pathOptions highlightOptions labelOptions
#' @importFrom sf st_read st_transform st_zm
#' @noRd
addPolygonsAndLines <- function(mapProxy, zoomLevel) {
    Tame_shapefile <- sf::st_read(
        "./inst/extdata/Upper_Tame_Wbs_Complete_SubCtchmnts_Dsslvd.shp"
    ) %>%
        sf::st_transform(crs = 4326)
    Tame_river_shapefile <- sf::st_read(
        "./inst/extdata/Tame_OS_WatercourseLink.shp"
    ) %>%
        sf::st_zm(Tame_river_shapefile) %>%
        sf::st_transform(crs = 4326)

    mapProxy |> clearGroup("polygons") |> clearGroup("lines")
    if (!is.null(zoomLevel)) {
        if (zoomLevel <= 13) {
            mapProxy |>
                addPolygons(
                    data = Tame_shapefile,
                    group = "polygons",
                    color = "black",
                    fillColor = "grey",
                    weight = 2,
                    opacity = 1,
                    fillOpacity = 0.3,
                    label = ~JWSb_ctmnt,
                    highlightOptions = highlightOptions(
                        weight = 6,
                        color = "blue",
                        fillOpacity = 0.3
                    ),
                    labelOptions = labelOptions(
                        style = list(
                            "font-size" = "15px",
                            "font-weight" = "bold",
                            "color" = "black"
                        ),
                        textsize = "15px",
                        direction = "auto"
                    ),
                    options = pathOptions(zIndex = 1)
                ) |>
                addPolylines(
                    data = Tame_river_shapefile,
                    group = "lines",
                    color = "blue",
                    weight = 1.5,
                    opacity = 0.7,
                    options = pathOptions(zIndex = 1)
                )
        }
    }
}
