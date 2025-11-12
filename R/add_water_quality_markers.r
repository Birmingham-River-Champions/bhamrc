#'
#' Add water quality markers to a Leaflet map proxy
#' @param mapProxy A leaflet map proxy object.
#' @param wq_data A list containing two data frames: all observations and recent averages.
#' @param wq_data_recent A data frame containing recent average water quality data for generating the ggplot graphs.
#' @param input Shiny input object for reactive inputs.
#' @description This function adds water quality markers to a Leaflet map proxy with popups containing ggplot graphs.
#' @importFrom leaflet clearPopups clearGroup addCircleMarkers popupOptions pathOptions colorBin addLegend addPopups
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal scale_fill_brewer xlab ylab scale_x_date theme element_text ggtitle
#' @importFrom stringr str_wrap
#' @importFrom leafpop popupGraph
#' @noRd
addWaterQualityMarkers <- function(
    mapProxy,
    wq_data_recent,
    wq_data,
    input
) {
    metric <- input$readingType
    reading_type_name <- unlist(names(which(water_quality_bw == metric)))
    wq_data_recent_map <- wq_data_recent |>
        drop_na()

    current_breaks <- filter(plot_breaks, reading_type == metric) |>
        select(bin_breaks) |>
        unlist()

    wq_data <- wq_data |>
        filter(!is.na(value))

    if (metric != "temperature_C") {
        pal_name <- "Blues"
        pal <- colorBin(
            palette = pal_name,
            pretty = FALSE,
            bins = current_breaks,
            reverse = FALSE
        )
        pal_values <- brewer.pal(
            length(attr(pal, "colorArgs")$bins),
            pal_name
        )
    } else {
        pal_name = "RdBu"
        pal <- colorBin(
            palette = pal_name,
            bins = current_breaks,
            pretty = FALSE,
            reverse = TRUE
        )
        pal_values <- rev(brewer.pal(
            length(attr(pal, "colorArgs")$bins),
            pal_name
        ))
    }

    mapProxy |> clearPopups() |> clearGroup("points")

    if (!is.null(wq_data_recent_map) && nrow(wq_data_recent_map) > 0) {
        plotPopups <- function(i, popup_width) {
            # Filter the data to include all observations only from the specific site
            site_id <- wq_data_recent_map$sampling_site[i]
            wqdata_SiteID <- filter(
                wq_data,
                sampling_site == site_id
            )
            organisation <- wq_data_recent_map$organisation[1]
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
                wqdata_SiteID$survey_date,
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
                "Water quality at ",
                site_id,
                ". Sampled by ",
                organisation,
                "."
            )
            p <- ggplot(
                wqdata_SiteID,
                aes(
                    x = as.Date(survey_date),
                    y = value,
                    fill = cut(
                        value,
                        breaks = current_breaks,
                        labels = pal_values[-1]
                    )
                )
            ) +
                geom_point(
                    size = 5,
                    pch = 21,
                    colour = "black"
                ) +
                scale_fill_manual(
                    name = reading_type_name,
                    values = pal_values,
                    drop = FALSE
                ) +
                theme_minimal() +
                xlab("Survey Date") +
                ylab(reading_type_name) +
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
            } else {
                popup_width <- 600
                popup_height <- 350
            }

            # Loop through recent averaged data to create point popups
            plots <- lapply(1:nrow(wq_data_recent_map), function(i) {
                plotPopups(i, popup_width)
            })
            mapProxy |>
                addCircleMarkers(
                    data = wq_data_recent_map,
                    lng = ~LONG,
                    lat = ~LAT,
                    radius = 6,
                    weight = 2,
                    fillColor = ~ pal(value),
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
                ) |>
                addLegend(
                    position = "topright",
                    pal = pal,
                    values = wq_data_recent_map$value,
                    title = reading_type_name,
                    group = "points",
                    opacity = 0.75
                )
        })
    } else {
        # Handle no data case
        if (
            all(is.na(wq_data_recent_map$LONG)) ||
                all(is.na(wq_data_recent_map$LAT))
        ) {
            default_lng <- -1.89983 # Example: center of Birmingham
            default_lat <- 52.48624 # Example: center of Birmingham
        } else {
            default_lng <- mean(wq_data_recent_map$LONG, na.rm = TRUE)
            default_lat <- mean(wq_data_recent_map$LAT, na.rm = TRUE)
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
    }
    # Clear existing points before adding new ones
    mapProxy |> clearGroup("points")
}
