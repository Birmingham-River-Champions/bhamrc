#' This function adds Invasive species markers to the map.
#' @param mapProxy A leaflet map proxy object.
#' @param data A data frame containing the Invasive species data to be plotted.
#' @param invasiveType The specific Invasive species to filter and plot.
#' @param plot_palette A vector of colors to use for the invasive species markers.
#' @importFrom leaflet clearPopups clearGroup addCircleMarkers addPopups popupOptions pathOptions colorFactor
#' @importFrom RColorBrewer brewer.pal
addInvasiveSpeciesMarkers <- function(
    mapProxy,
    data,
    invasiveType,
    plot_palette
) {
    mapProxy |> clearPopups() |> clearGroup("points")

    # Define the color palette and domain
    invasivePalette <- c(
        plot_palette[1],
        plot_palette[2],
        plot_palette[3],
        plot_palette[4],
        plot_palette[1],
        plot_palette[3]
    )

    invasiveDomain <- levels(data$abundance)

    pal <- colorFactor(
        palette = invasivePalette,
        domain = invasiveDomain,
        ordered = TRUE
    )

    # Filter data based on invasive species type
    if (invasiveType %in% c("signal_crayfish", "killer_demon_shrimp")) {
        data_filtered <- data |>
            filter(
                measurement %in%
                    invasiveType &
                    abundance %in% c(">1000", "100-999", "10-99", "1-9")
            )
        legend_colors <- c(
            plot_palette[1],
            plot_palette[2],
            plot_palette[3],
            plot_palette[4]
        )

        legend_labels <- c(
            ">1000",
            "100-999",
            "10-99",
            "1-9"
        )
    } else if (
        invasiveType %in%
            c("himalayan_balsam", "japanese_knotweed", "giant_hogweed")
    ) {
        data_filtered <- data |>
            filter(
                measurement %in%
                    invasiveType &
                    abundance %in%
                        c("Abundant (>33%)", "Present (1-33%)")
            )

        legend_colors <- c(
            plot_palette[1],
            plot_palette[3]
        )

        legend_labels <- c(
            "Abundant (>33%)",
            "Present (1-33%)"
        )
    } else {
        data_filtered <- NULL
    }

    # Clear existing points before adding new ones
    mapProxy |> clearGroup("points")

    # Check if there is data to display
    if (!is.null(data_filtered) && nrow(data_filtered) > 0) {
        mapProxy |>
            addCircleMarkers(
                data = data_filtered,
                lng = ~LONG,
                lat = ~LAT,
                popup = lapply(1:nrow(data_filtered), function(i) {
                    content <- paste0(
                        "<div style='overflow: auto; width: auto; max-width: 300px;'>",
                        "<strong style='font-size:16px;'>Highest value in the last 3 years</strong>",
                        "<table style='width:100%; white-space: nowrap; margin-top: 10px;'>",
                        "<tr><td><strong>Organisation:</strong></td><td>",
                        data_filtered$organisation[i],
                        "</td></tr>",
                        "<tr><td><strong>BRC site ID:</strong></td><td>",
                        data_filtered$sampling_site[i],
                        "</td></tr>",
                        "<tr><td><strong>Survey date:</strong></td><td>",
                        data_filtered$date_time[i],
                        "</td></tr>",
                        "<tr><td><strong>Invasive species:</strong></td><td>",
                        data_filtered$measurement[i],
                        "</td></tr>",
                        "<tr><td><strong>Abundance:</strong></td><td>",
                        data_filtered$abundance[i],
                        "</td></tr>",
                        "</table>",
                        "</div>"
                    )
                    return(content)
                }),
                radius = 6,
                weight = 2,
                fillColor = ~ pal(abundance),
                color = "black",
                stroke = TRUE,
                opacity = 0.5,
                fill = TRUE,
                fillOpacity = 1,
                group = "points",
                options = pathOptions(zIndex = 2)
            ) |>
            addLegend(
                position = "topright",
                colors = legend_colors,
                labels = legend_labels,
                values = data_filtered$abundance,
                title = "Abundance",
                opacity = 0.75,
                group = "points"
            )
    } else {
        # If no records, add a popup message in the center of the map
        if (all(is.na(data$LONG)) || all(is.na(data$LAT))) {
            # Default to some center coordinates (use your map's center or a known location)
            default_lng <- -1.89983 # Example: center of Birmingham
            default_lat <- 52.48624 # Example: center of Birmingham
        } else {
            # Calculate mean coordinates (excluding NAs)
            default_lng <- mean(data$LONG, na.rm = TRUE)
            default_lat <- mean(data$LAT, na.rm = TRUE)
        }

        # Add popup with default coordinates
        mapProxy |>
            addPopups(
                lng = default_lng,
                lat = default_lat,
                popup = "<div style='text-align:center;'><strong>No records of this invasive species to date.</strong></div>",
                options = popupOptions(
                    closeButton = TRUE,
                    closeOnClick = FALSE
                )
            )
    }
}
