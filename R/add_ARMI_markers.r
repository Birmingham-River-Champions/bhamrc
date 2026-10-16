#' Add ARMI markers to the map.
#' @param mapProxy A leaflet map proxy object.
#' @param map_data A data frame containing the site-averaged recent ARMI data to be plotted on the map.
#' @param popup_data Retained for compatibility; plots are generated when map
#'   markers are clicked.
#' @param screen_width Retained for compatibility; popup dimensions are
#'   selected when a marker is clicked.
#' @importFrom leaflet clearGroup addCircleMarkers popupOptions pathOptions colorBin addLegend showGroup
#' @importFrom RColorBrewer brewer.pal
addARMIMarkers <- function(mapProxy, map_data, popup_data, screen_width) {
    breaks_vector <- filter(plot_breaks, reading_type == "ARMI") |>
        select(bin_breaks) |>
        unlist()

    pal_name <- "RdBu"
    pal <- colorBin(
        palette = pal_name,
        #domain = map_data$ARMI,
        bins = breaks_vector,
        pretty = FALSE
    )

    pal_values <- brewer.pal(
        length(attr(pal, "colorArgs")$bins),
        pal_name
    )

    mapProxy |> clearGroup("ARMI points")

    if (!is.null(map_data) && nrow(map_data) > 0) {
        mapProxy |>
            addCircleMarkers(
                # Keep IDs aligned with map_data rows so the click handler can
                # resolve a marker back to its site's full ARMI time series.
                layerId = ~ seq_len(nrow(map_data)),
                data = map_data,
                lng = ~LONG,
                lat = ~LAT,
                radius = 6,
                weight = 2,
                fillColor = ~ pal(ARMI),
                color = "black",
                stroke = TRUE,
                opacity = 0.5,
                fill = TRUE,
                fillOpacity = 1,
                group = "ARMI points"
            ) |>
            addLegend(
                position = "topright",
                values = map_data$ARMI,
                colors = rev(pal_values[-length(pal_values)]),
                labels = rev(c(
                    paste0("<", breaks_vector[2]),
                    paste0(breaks_vector[2], " - ", breaks_vector[3]),
                    paste0(breaks_vector[3], " - ", breaks_vector[4]),
                    paste0(breaks_vector[4], " - ", breaks_vector[5]),
                    paste0(">", breaks_vector[5])
                )),
                title = "ARMI Score",
                group = "legend",
                opacity = 0.75
            )
    }
}
