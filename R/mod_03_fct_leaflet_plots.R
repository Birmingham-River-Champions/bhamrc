# Define all helper functions before they are called
# These functions are used in mod_03_plot_data.R
#' Helper functions for leaflet plots

#' This function clears the map layers
#' @param mapProxy A leaflet map proxy object.
#' @importFrom leaflet clearMarkers clearControls clearGroup addPolygons addPolylines addCircleMarkers popupGraph addPopups popupOptions pathOptions
#' @noRd
clearMapLayers <- function(mapProxy) {
    mapProxy |>
        clearMarkers() |>
        clearControls() |>
        clearGroup("polygons") |>
        clearGroup("lines") |>
        clearGroup("points")
}

#' This function adds polygons and lines to the map based on the zoom level.
#' @param mapProxy A leaflet map proxy object.
#' @param zoomLevel The current zoom level of the map.
#' @importFrom leaflet clearGroup addPolygons addPolylines pathOptions highlightOptions labelOptions
#' @noRd
addPolygonsAndLines <- function(mapProxy, zoomLevel) {
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

#' This function adds ARMI markers to the map with popups containing ggplot graphs.
#' @param mapProxy A leaflet map proxy object.
#' @param data A data frame containing the ARMI data to be plotted.
#' @param riverflyARMIData A data frame containing all ARMI data for generating the ggplot graphs.
#' @importFrom leaflet clearGroup addCircleMarkers popupOptions pathOptions
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal scale_fill_manual xlab ylab scale_x_date theme element_text ggtitle
#' @importFrom stringr str_wrap
#' @noRd
addARMIMarkers <- function(mapProxy, data, riverflyARMIData) {
    pal <- colorFactor(
        palette = levels(data$ARMI_Plot_Colour),
        domain = data$ARMI_Plot_Colour
    )

    mapProxy |> clearGroup("points")

    if (!is.null(data) && nrow(data) > 0) {
        plotPopups <- function(i, popup_width) {
            site_id <- data$`BRC site ID`[i]
            organisation <- data$Organisation[i]

            riverflyARMIData_SiteID <- filter(
                riverflyARMIData,
                `BRC site ID` == site_id
            )
            ##Some organisations don't sound right with "the" in front
            organisation <- if (
                organisation != "Hall Green's Keepin' It Clean" &
                    organisation != "Birmingham Conservation Society"
            ) {
                organisation <- paste("the", organisation)
            } else {
                organisation <- organisation # This line is optional, just for clarity
            }

            # Calculate date range buffer if there's only one sample
            date_range <- range(
                riverflyARMIData_SiteID$`Survey date`,
                na.rm = TRUE
            )
            if (diff(date_range) == 0) {
                date_range <- c(date_range[1] - 15, date_range[2] + 15)
            }

            # Set character width for str_wrap based on popup width
            title_wrap_width <- ifelse(
                popup_width <= 300,
                37,
                ifelse(popup_width <= 450, 50, 75)
            )
            title_text <- paste0(
                "ARMI score at ",
                site_id,
                ". Sampled by ",
                organisation,
                "."
            )

            p <- ggplot(
                riverflyARMIData_SiteID,
                aes(
                    x = as.Date(`Survey date`),
                    y = ARMI,
                    fill = cut(
                        ARMI,
                        breaks = c(-Inf, 5:14, Inf),
                        labels = c(
                            "≤5",
                            "6",
                            "7",
                            "8",
                            "9",
                            "10",
                            "11",
                            "12",
                            "13",
                            "14",
                            "≥15"
                        )
                    )
                )
            ) +
                geom_point(size = 5, pch = 21, colour = "black") +
                theme_minimal() +
                scale_fill_manual(
                    name = "ARMI",
                    values = brewer.pal(n = 11, name = "RdBu"),
                    drop = FALSE
                ) +
                xlab("Survey Date") +
                ylab("ARMI Score") +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y",
                    limits = date_range
                ) +
                theme(
                    plot.title.position = "plot",
                    axis.title.x = element_text(face = "bold"),
                    axis.title.y = element_text(face = "bold"),
                    legend.position = "none",
                    plot.title = element_text(
                        size = 13,
                        face = "bold",
                        hjust = 0.5
                    )
                ) + ##Did have the text over the y-axis title, but changed to centre - ","
                ggtitle(str_wrap(title_text, width = title_wrap_width)) # Wrap the title based on width

            return(p)
        }

        # Adjust plot size based on screen width
        observe({
            if (!is.null(input$screen_width)) {
                if (input$screen_width <= 480) {
                    # For small screens like iPhones
                    popup_width <- 300
                    popup_height <- 250
                } else if (input$screen_width <= 768) {
                    # For tablets
                    popup_width <- 400
                    popup_height <- 275
                } else {
                    # For larger screens
                    popup_width <- 600
                    popup_height <- 350
                }
            }

            plots <- lapply(1:nrow(data), function(i) {
                plotPopups(i, popup_width)
            })

            mapProxy |>
                addCircleMarkers(
                    data = data,
                    lng = ~LONG,
                    lat = ~LAT,
                    radius = 6,
                    weight = 2,
                    fillColor = ~ pal(ARMI_Plot_Colour),
                    color = "black",
                    stroke = TRUE,
                    opacity = 0.5,
                    fill = TRUE,
                    fillOpacity = 1,
                    group = "points",
                    popup = popupGraph(
                        plots,
                        width = popup_width,
                        height = popup_height
                    )
                )
        })
    }
}

#' This function adds Urban Riverfly species markers to the map with popups containing ggplot graphs.
#' @param mapProxy A leaflet map proxy object.
#' @param data A data frame containing the Urban Riverfly species data to be plotted.
#' @param riverflyspeciesData A data frame containing all Urban Riverfly species data for generating the ggplot graphs.
#' @param taxaType The specific Urban Riverfly species to filter and plot.
#' @importFrom leaflet clearPopups clearGroup addCircleMarkers popupGraph popupOptions pathOptions
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal scale_fill_manual xlab ylab scale_x_date scale_y_continuous theme element_text ggtitle
#' @importFrom stringr str_wrap
#' @noRd
addRiverflySpeciesMarkers <- function(
    mapProxy,
    data,
    riverflyspeciesData,
    taxaType
) {
    mapProxy |> clearPopups() |> clearGroup("points")

    riverflyspeciesData_Recent_Map <- data |> filter(Taxa == taxaType)
    pal <- colorFactor(
        palette = levels(
            riverflyspeciesData_Recent_Map$Riverfly_Species_Colour
        ),
        domain = riverflyspeciesData_Recent_Map$Riverfly_Species_Colour
    )

    # Clear existing points before adding new ones
    mapProxy |> clearGroup("points")

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
        # Proceed if riverflyspeciesData_Recent_Map data is available
        plotPopups <- function(i, popup_width) {
            site_id <- riverflyspeciesData_Recent_Map$`BRC site ID`[i]
            organisation <- riverflyspeciesData_Recent_Map$Organisation[
                i
            ]

            riverflyspeciesData_All_ggplot <- filter(
                riverflyspeciesData,
                `BRC site ID` == site_id & Taxa == taxaType
            )
            # Custom changing of some organisations (those ) for "Flat bodied stone clinger mayfly"
            organisation <- if (
                organisation != "Hall Green's Keepin' It Clean" &
                    organisation != "Birmingham Conservation Society"
            ) {
                organisation <- paste("the", organisation)
            } else {
                organisation <- organisation # This line is optional, just for clarity
            }
            # Custom shortening for "Flat bodied stone clinger mayfly"
            taxaType_CommonName <- gsub(
                "\\s*\\([^\\)]+\\)",
                "",
                taxaType
            )
            if (taxaType_CommonName == "Flat-bodied stone clinger mayfly") {
                taxaType_CommonName <- "Stone clinger mayfly"
            }

            # Calculate date range buffer if there's only one sample
            date_range <- range(
                riverflyspeciesData_All_ggplot$`Survey date`,
                na.rm = TRUE
            )
            if (diff(date_range) == 0) {
                date_range <- c(date_range[1] - 15, date_range[2] + 15)
            }

            title_wrap_width <- ifelse(
                popup_width <= 300,
                37,
                ifelse(popup_width <= 450, 50, 75)
            )
            title_text <- paste0(
                taxaType_CommonName,
                " numbers at ",
                site_id,
                ". Sampled by ",
                organisation,
                "."
            )

            p <- ggplot(
                riverflyspeciesData_All_ggplot,
                aes(
                    x = as.Date(`Survey date`),
                    y = Abundance,
                    fill = cut(
                        Abundance,
                        breaks = c(-Inf, 0:4),
                        labels = c(
                            "0",
                            "1-9",
                            "10-99",
                            "100-999",
                            ">1000"
                        )
                    )
                )
            ) +
                geom_point(size = 5, pch = 21, colour = "black") +
                theme_minimal() +
                scale_fill_manual(
                    values = brewer.pal(n = 5, name = "Greys"),
                    drop = FALSE
                ) +
                xlab("Date") +
                ylab("Abundance") +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y",
                    limits = date_range
                ) + # Set date limits with buffer
                scale_y_continuous(
                    breaks = c(0, 1, 2, 3, 4), # Custom breaks for y-axis
                    labels = c("0", "1-9", "10-99", "100-999", ">1000"),
                    limits = c(0, 4)
                ) + # Custom labels
                theme(
                    plot.title.position = "plot",
                    axis.title.x = element_text(face = "bold"),
                    axis.title.y = element_text(face = "bold"),
                    legend.position = "none",
                    plot.title = element_text(
                        size = 13,
                        face = "bold",
                        hjust = 0.5
                    )
                ) + ##Did have the text over the y-axis title, but changed to centre - "","
                ggtitle(str_wrap(title_text, width = title_wrap_width))

            return(p)
        }

        observe({
            if (!is.null(input$screen_width)) {
                if (input$screen_width <= 480) {
                    popup_width <- 300
                    popup_height <- 250
                } else if (input$screen_width <= 768) {
                    popup_width <- 400
                    popup_height <- 275
                } else {
                    popup_width <- 600
                    popup_height <- 350
                }
            }

            # Generate a plot for each marker based on the filtered data
            plots <- lapply(
                1:nrow(riverflyspeciesData_Recent_Map),
                function(i) {
                    plotPopups(i, popup_width)
                }
            )

            # Add markers to the map using riverflyspeciesData_Recent_Map for points
            mapProxy |>
                addCircleMarkers(
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
                    group = "points",
                    popup = popupGraph(
                        plots,
                        width = popup_width,
                        height = popup_height
                    )
                )
        })
    }
}

#' This function adds Other species markers to the map.
#' @param mapProxy A leaflet map proxy object.
#' @param data A data frame containing the Other species data to be plotted.
#' @param otherSpecies The specific Other species to filter and plot.
#' @importFrom leaflet clearGroup addCircleMarkers popupGraph popupOptions pathOptions
#' @importFrom RColorBrewer brewer.pal
#' @noRd
addOtherSpeciesMarkers <- function(mapProxy, data, otherSpecies) {
    pal <- colorFactor(
        palette = brewer.pal(n = 5, name = "Greys")[5:2],
        domain = levels(data$Abundance),
        ordered = TRUE
    ) ##Abundance levels in opposite order here
    mapProxy |> clearGroup("points") ##Did it this way originally for the legend so higher abundances were on top, but scrapped the legend as it covered plots for other input

    data$Taxa <- gsub("\\s*\\([^\\)]+\\)", "", data$Taxa)

    if (!is.null(data) && nrow(data) > 0) {
        mapProxy |>
            addCircleMarkers(
                data = data,
                lng = ~LONG,
                lat = ~LAT,
                popup = lapply(1:nrow(data), function(i) {
                    content <- paste0(
                        "<div style='overflow: auto; width: auto; max-width: 300px;'>",
                        "<strong style='font-size:16px;'>Highest value in the last 3 years</strong>",
                        "<table style='width:100%; white-space: nowrap; margin-top: 10px;'>",
                        "<tr><td><strong>Organisation:</strong></td><td>",
                        data$Organisation[i],
                        "</td></tr>",
                        "<tr><td><strong>BRC site ID:</strong></td><td>",
                        data$`BRC site ID`[i],
                        "</td></tr>",
                        "<tr><td><strong>Survey date:</strong></td><td>",
                        data$`Survey date`[i],
                        "</td></tr>",
                        "<tr><td><strong>Species:</strong></td><td>",
                        data$Taxa[i],
                        "</td></tr>",
                        "<tr><td><strong>Abundance:</strong></td><td>",
                        data$Abundance[i],
                        "</td></tr>",
                        "</table>",
                        "</div>"
                    )
                    return(content)
                }),
                radius = 6,
                weight = 2,
                fillColor = ~ pal(Abundance),
                color = "black",
                stroke = TRUE,
                opacity = 0.5,
                fill = TRUE,
                fillOpacity = 1,
                group = "points",
                options = pathOptions(zIndex = 2)
            )
    }
}

#' This function adds Invasive species markers to the map.
#' @param mapProxy A leaflet map proxy object.
#' @param data A data frame containing the Invasive species data to be plotted.
#' @param invasiveType The specific Invasive species to filter and plot.
#' @importFrom leaflet clearPopups clearGroup addCircleMarkers addPopups popupOptions pathOptions
#' @importFrom RColorBrewer brewer.pal
#' @noRd
addInvasiveSpeciesMarkers <- function(mapProxy, data, invasiveType) {
    mapProxy |> clearPopups() |> clearGroup("points")

    # Define the color palette and domain
    invasivePalette <- c(
        brewer.pal(n = 9, name = "RdBu")[1],
        brewer.pal(n = 9, name = "RdBu")[2],
        brewer.pal(n = 9, name = "RdBu")[3],
        brewer.pal(n = 9, name = "RdBu")[4],
        brewer.pal(n = 9, name = "RdBu")[1],
        brewer.pal(n = 9, name = "RdBu")[3]
    )

    invasiveDomain <- c(
        ">1000",
        "100-999",
        "10-99",
        "1-9",
        "Abundant (>33%)",
        "Present (1-33%)"
    )

    pal <- colorFactor(
        palette = invasivePalette,
        domain = invasiveDomain,
        ordered = TRUE
    )

    # Filter data based on invasive species type
    if (invasiveType %in% c("Signal crayfish", "Killer or demon shrimp")) {
        data_filtered <- data |>
            filter(
                `Invasive species` %in%
                    invasiveType &
                    Abundance %in% c(">1000", "100-999", "10-99", "1-9")
            )
    } else if (
        invasiveType %in%
            c("Himalayan balsam", "Japanese knotweed", "Giant hogweed")
    ) {
        data_filtered <- data |>
            filter(
                `Invasive species` %in%
                    invasiveType &
                    Abundance %in%
                        c("Abundant (>33%)", "Present (1-33%)")
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
                        data_filtered$Organisation[i],
                        "</td></tr>",
                        "<tr><td><strong>BRC site ID:</strong></td><td>",
                        data_filtered$`BRC site ID`[i],
                        "</td></tr>",
                        "<tr><td><strong>Survey date:</strong></td><td>",
                        data_filtered$`Survey date`[i],
                        "</td></tr>",
                        "<tr><td><strong>Invasive species:</strong></td><td>",
                        data_filtered$`Invasive species`[i],
                        "</td></tr>",
                        "<tr><td><strong>Abundance:</strong></td><td>",
                        data_filtered$Abundance[i],
                        "</td></tr>",
                        "</table>",
                        "</div>"
                    )
                    return(content)
                }),
                radius = 6,
                weight = 2,
                fillColor = ~ pal(Abundance),
                color = "black",
                stroke = TRUE,
                opacity = 0.5,
                fill = TRUE,
                fillOpacity = 1,
                group = "points",
                options = pathOptions(zIndex = 2)
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
