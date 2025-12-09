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
        leaflet::hideGroup("Riverfly points") |>
        leaflet::hideGroup("Invasive points") |>
        leaflet::hideGroup("Other spp points") |>
        leaflet::hideGroup("ARMI points") |>
        leaflet::hideGroup("Water Quality points") |>
        leaflet::removeControl("legend")
    print("Map layers cleared (used removeControl())")
}
