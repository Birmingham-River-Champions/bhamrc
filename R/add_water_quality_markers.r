#'
#' Add water quality markers to a Leaflet map proxy
#' @param mapProxy A leaflet map proxy object.
#' @param wq_data_list A list containing two data frames: all observations and recent averages.
#' @param input Shiny input object for reactive inputs.
#' @importFrom leaflet clearPopups clearGroup addCircleMarkers popupOptions pathOptions colorFactor
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal scale_fill_manual xlab ylab scale_x_date theme element_text ggtitle
#' @importFrom stringr str_wrap
#' @importFrom leafpop popupGraph
#' @noRd
addWaterQualityMarkers <- function(
    mapProxy,
    wq_data_list,
    input
) {
    wq_data <- wq_data_list$all_obs
    wq_data_recent <- wq_data_list$recent

    reading_type <- input$readingType
    pal <- colorFactor(
        palette = levels(wq_data$WQ_Plot_Colour),
        domain = wq_data$WQ_Plot_Colour
    )

    wq_data <- wq_data |>
        filter(!is.na(value))

    current_breaks <- plot_breaks$bin_breaks[
        plot_breaks$metric == reading_type
    ]

    mapProxy |> clearPopups() |> clearGroup("points")

    if (!is.null(wq_data) && nrow(wq_data) > 0) {
        plotPopups <- function(i, popup_width) {
            site_id <- wq_data$sampling_site[i]
            organisation <- wq_data$organisation[i]

            wqdata_SiteID <- filter(
                wq_data,
                sampling_site == site_id
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
                    fill = WQ_Plot_Colour
                )
            ) +
                geom_point(size = 5, pch = 21, colour = "black") +
                #geom_line(color = wqdata_SiteID$WQ_Plot_Colour) +
                theme_minimal() +
                scale_fill_manual(
                    name = reading_type,
                    values = brewer.pal(n = 9, name = "Blues"),
                    drop = FALSE
                ) +
                xlab("Survey Date") +
                ylab(names(which(water_quality_bw == reading_type))) +
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

            plots <- lapply(1:nrow(wq_data), function(i) {
                plotPopups(i, popup_width)
            })

            mapProxy |>
                addCircleMarkers(
                    data = wq_data_recent,
                    lng = ~LONG,
                    lat = ~LAT,
                    radius = 6,
                    weight = 2,
                    fillColor = ~ pal(WQ_Plot_Colour),
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
    # Clear existing points before adding new ones
    mapProxy |> clearGroup("points")
}
