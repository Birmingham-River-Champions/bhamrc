#' This function adds Other species markers to the map.
#' @param mapProxy A leaflet map proxy object.
#' @param fullData A data frame containing the Other species data to be plotted.
#' @param otherSpecies The specific Other species to filter and plot.
#' @importFrom leaflet clearGroup addCircleMarkers popupOptions pathOptions colorFactor
#' @importFrom RColorBrewer brewer.pal
addOtherSpeciesMarkers <- function(mapProxy, fullData, otherSpecies) {
    pal <- colorFactor(
        palette = brewer.pal(n = 5, name = "Greys")[5:2],
        domain = levels(fullData$abundance),
        ordered = TRUE
    ) ##Abundance levels in opposite order here
    mapProxy |> clearGroup("Other spp points") ##Did it this way originally for the legend so higher abundances were on top, but scrapped the legend as it covered plots for other input
    data <- fullData |>
        filter(!is.na(abundance))
    if (!(is.null(data)) && (nrow(data) > 0)) {
        mapProxy |>
            addCircleMarkers(
                data = data,
                lng = ~LONG,
                lat = ~LAT,
                popup = lapply(1:nrow(data), function(i) {
                    selectedTaxa <- other_spp_bw[[data$taxa[i]]]
                    selectedTaxa <- gsub("\\s*\\([^\\)]+\\)", "", selectedTaxa)

                    content <- paste0(
                        "<div style='overflow: auto; width: auto; max-width: 300px;'>",
                        "<strong style='font-size:16px;'>Highest value in the last 3 years</strong>",
                        "<table style='width:100%; white-space: nowrap; margin-top: 10px;'>",
                        "<tr><td><strong>Organisation:</strong></td><td>",
                        data$organisation[i],
                        "</td></tr>",
                        "<tr><td><strong>BRC site ID:</strong></td><td>",
                        data$sampling_site[i],
                        "</td></tr>",
                        "<tr><td><strong>Survey date:</strong></td><td>",
                        data$survey_date[i],
                        "</td></tr>",
                        "<tr><td><strong>Species:</strong></td><td>",
                        selectedTaxa,
                        "</td></tr>",
                        "<tr><td><strong>Abundance:</strong></td><td>",
                        data$abundance[i],
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
                group = "Other spp points",
                options = pathOptions(zIndex = 2)
            ) |>
            addLegend(
                position = "topright",
                pal = pal,
                values = data$abundance,
                title = "Abundance",
                opacity = 0.75,
                group = "Other spp points"
            )
    }
}
