#' This function adds ARMI markers to the map with popups containing ggplot graphs.
#' @param mapProxy A leaflet map proxy object.
#' @param data A data frame containing the site-averaged recent ARMI data to be plotted.
#' @param riverflyARMIData A data frame containing all ARMI data for generating the ggplot graphs.
#' @param input The Shiny input object to access screen width for responsive popup sizing.
#' @importFrom leaflet clearGroup addCircleMarkers popupOptions pathOptions colorBin addLegend showGroup
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_minimal scale_fill_manual xlab ylab scale_x_date theme element_text ggtitle
#' @importFrom stringr str_wrap
#' @importFrom leafpop popupGraph
addARMIMarkers <- function(mapProxy, map_data, popup_data, screen_width) {
    breaks_vector <- filter(plot_breaks, reading_type == "ARMI") |>
        select(bin_breaks) |>
        unlist()

    pal_name <- "Blues"
    pal <- colorBin(
        palette = pal_name,
        domain = map_data$ARMI,
        bins = breaks_vector,
        pretty = FALSE
    )

    pal_values <- brewer.pal(
        length(attr(pal, "colorArgs")$bins),
        pal_name
    )

    mapProxy |> clearGroup("ARMI points")

    if (!is.null(map_data) && nrow(map_data) > 0) {
        plotPopups <- function(i, popup_width) {
            site_id <- map_data$sampling_site[i]
            organisation <- map_data$organisation[i]

            # Get all ARMI data for this specific site
            riverflyARMIData_SiteID <- popup_data[[site_id]]

            # Some organisations don't sound right with "the" in front
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
                riverflyARMIData_SiteID$survey_date,
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
                    x = as.Date(survey_date),
                    y = ARMI,
                    fill = cut(
                        ARMI,
                        breaks = breaks_vector,
                        labels = c(brewer.pal(n = 5, name = "Blues"))
                    )
                )
            ) +
                geom_point(size = 5, pch = 21, colour = "black") +
                theme_minimal() +
                scale_fill_manual(
                    name = "ARMI",
                    values = brewer.pal(n = 6, name = "Blues"),
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
        if (!is.null(screen_width)) {
            if (screen_width <= 480) {
                # For small screens like iPhones
                popup_width <- 300
                popup_height <- 250
            } else if (screen_width <= 768) {
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

        # Loop through the site-averaged map points and create a popup ggplot for each
        plots <- lapply(1:nrow(map_data), function(i) {
            plotPopups(i, popup_width)
        })

        mapProxy |>
            addCircleMarkers(
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
                group = "ARMI points",
                popup = popupGraph(
                    plots,
                    width = popup_width,
                    height = popup_height
                )
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
