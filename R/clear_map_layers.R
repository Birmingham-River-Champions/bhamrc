# Define all helper functions before they are called
# These functions are used in mod_03_plot_data.R
#' Helper functions for leaflet plots

#' This function clears the map layers
#' @param mapProxy A leaflet map proxy object.
#' @importFrom leaflet clearMarkers clearControls clearGroup addPolygons addPolylines addCircleMarkers addPopups popupOptions pathOptions hideGroup
#' @noRd
clearMapLayers <- function(mapProxy) {
    mapProxy |>
        clearControls() |>
        hideGroup("Riverfly points") |>
        hideGroup("Invasive points") |>
        hideGroup("Other spp points") |>
        hideGroup("ARMI points") |>
        hideGroup("Water Quality points")
    print("Map layers cleared")
}
