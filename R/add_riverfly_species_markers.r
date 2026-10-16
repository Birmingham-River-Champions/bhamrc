#' Add Urban Riverfly species markers to the map.
#' @param mapProxy A leaflet map proxy object.
#' @param map_data A data frame containing the Urban Riverfly species data to be plotted on the map.
#' @param popup_data Retained for compatibility; plots are generated when map
#'   markers are clicked.
#' @param taxaType The specific Urban Riverfly species to filter and plot.
#' @param screen_width Retained for compatibility; popup dimensions are
#'   selected when a marker is clicked.
#' @importFrom leaflet clearPopups clearGroup addCircleMarkers popupOptions pathOptions colorFactor
#' @importFrom RColorBrewer brewer.pal
addRiverflySpeciesMarkers <- function(
    mapProxy,
    map_data,
    popup_data,
    taxaType,
    screen_width
) {
    mapProxy |>
        clearPopups() |>
        clearGroup("Riverfly points")

    riverflyspeciesData_Recent_Map <- map_data |>
        drop_na()

    pal <- colorFactor(
        palette = levels(
            riverflyspeciesData_Recent_Map$Riverfly_Species_Colour
        ),
        domain = riverflyspeciesData_Recent_Map$Riverfly_Species_Colour
    )

    # If no records for the specific taxaType, display popup message
    if (nrow(riverflyspeciesData_Recent_Map) == 0) {
        # Handle no data case
        if (all(is.na(data$LONG)) || all(is.na(data$LAT))) {
            default_lng <- -1.89983 # Example: center of Birmingham
            default_lat <- 52.48624 # Example: center of Birmingham
        } else {
            default_lng <- mean(data$LONG, na.rm = TRUE)
            default_lat <- mean(data$LAT, na.rm = TRUE)
        }

        mapProxy |>
            addPopups(
                lng = default_lng,
                lat = default_lat,
                popup = "<div style='text-align:center;'><strong>No project records currently</strong></div>",
                options = popupOptions(
                    closeButton = TRUE,
                    closeOnClick = FALSE
                )
            )
    } else {
        # Add markers to the map using riverflyspeciesData_Recent_Map for points
        mapProxy |>
            addCircleMarkers(
                # IDs index the NA-filtered data actually plotted, which is
                # also the row order used to resolve clicks in the server.
                layerId = ~ seq_len(nrow(riverflyspeciesData_Recent_Map)),
                data = riverflyspeciesData_Recent_Map,
                lng = ~LONG,
                lat = ~LAT,
                radius = 6,
                weight = 2,
                fillColor = ~ pal(Riverfly_Species_Colour),
                color = "black",
                stroke = TRUE,
                opacity = 0.5,
                fill = TRUE,
                fillOpacity = 1,
                group = "Riverfly points"
            ) |>
            addLegend(
                position = "topright",
                values = riverflyspeciesData_Recent_Map$Riverfly_Species_Colour,
                colors = rev(brewer.pal(5, "Greys")),
                labels = rev(c(
                    "0",
                    "1-9",
                    "10-99",
                    "100-999",
                    ">1000"
                )),
                title = "Abundance",
                opacity = 0.75,
                group = "legend"
            )
    }
}
