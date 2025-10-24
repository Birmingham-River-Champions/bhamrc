#' This function adds Urban Riverfly species markers to the map with popups containing ggplot graphs.
#' @param mapProxy A leaflet map proxy object.
#' @param data A data frame containing the Urban Riverfly species data to be plotted.
#' @param riverflyspeciesData A data frame containing all Urban Riverfly species data for generating the ggplot graphs.
#' @param taxaType The specific Urban Riverfly species to filter and plot.
#' @importFrom leaflet clearPopups clearGroup addCircleMarkers popupOptions pathOptions colorFactor
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal scale_fill_manual xlab ylab scale_x_date scale_y_continuous theme element_text ggtitle
#' @importFrom stringr str_wrap
#' @importFrom leafpop popupGraph
#' @noRd
addRiverflySpeciesMarkers <- function(
    mapProxy,
    data,
    riverflyspeciesData,
    taxaType,
    input
) {
    mapProxy |> clearPopups() |> clearGroup("points")

    riverflyspeciesData_Recent_Map <- data |>
        filter(taxa == taxaType) |>
        drop_na()

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
            site_id <- riverflyspeciesData_Recent_Map$sampling_site[i]
            organisation <- riverflyspeciesData_Recent_Map$organisation[
                i
            ]

            riverflyspeciesData_All_ggplot <- filter(
                riverflyspeciesData,
                sampling_site == site_id & taxa == taxaType
            ) |>
                mutate(
                    abundance = case_when(
                        is.na(abundance) ~ 0,
                        .default = abundance
                    )
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
                riverfly_spp_bw[[taxaType]]
            )
            if (taxaType_CommonName == "Flat-bodied stone clinger mayfly") {
                taxaType_CommonName <- "Stone clinger mayfly"
            }

            # Calculate date range buffer if there's only one sample
            date_range <- range(
                riverflyspeciesData_All_ggplot$survey_date,
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
                    x = as.Date(survey_date),
                    y = abundance,
                    fill = cut(
                        abundance,
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
            } else {
                popup_width <- 600
                popup_height <- 350
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
