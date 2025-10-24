# Define all helper functions before they are called
# These functions are used in mod_03_plot_data.R
#' Helper functions for leaflet plots

#' This function clears the map layers
#' @param mapProxy A leaflet map proxy object.
#' @importFrom leaflet clearMarkers clearControls clearGroup addPolygons addPolylines addCircleMarkers addPopups popupOptions pathOptions
#' @noRd
clearMapLayers <- function(mapProxy) {
    mapProxy |>
        clearMarkers() |>
        clearControls() |>
        clearGroup("polygons") |>
        clearGroup("lines") |>
        clearGroup("points")
}
