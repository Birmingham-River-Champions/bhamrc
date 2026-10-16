#' Build an ARMI popup plot
#'
#' Plot one site's ARMI observations for display in a Leaflet marker popup.
#'
#' @param row_data A data frame with `survey_date` and `ARMI` columns.
#' @param breaks_vector Numeric interval breaks used to classify ARMI values.
#' @param date_range The date limits to display on the x-axis.
#' @param title_text Text to wrap and display as the plot title.
#' @param title_wrap_width Maximum character width for the wrapped title.
#' @param width Popup plot width in pixels.
#' @param height Popup plot height in pixels.
#' @return A Leaflet popup graph widget.
#' @importFrom ggplot2 aes element_text geom_point ggplot ggtitle scale_fill_manual scale_x_date scale_y_continuous theme theme_minimal xlab ylab
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringr str_wrap
#' @importFrom leafpop popupGraph
#' @keywords internal
#' @noRd
# These plot builders are based on the popup plots in the performance branch;
# they are called by the marker-click observer so rendering is deferred until
# a user asks to inspect a site's history.
make_armi_popup <- function(
    row_data,
    breaks_vector,
    date_range,
    title_text,
    title_wrap_width,
    width,
    height
) {
    plot <- ggplot(
        row_data,
        aes(
            x = as.Date(survey_date),
            y = ARMI,
            fill = cut(
                ARMI,
                breaks = breaks_vector,
                labels = brewer.pal(n = 5, name = "RdBu")
            )
        )
    ) +
        geom_point(size = 5, pch = 21, colour = "black") +
        theme_minimal() +
        scale_fill_manual(
            name = "ARMI",
            values = brewer.pal(n = 6, name = "RdBu"),
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
            ),
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        ggtitle(
            str_wrap(title_text, width = title_wrap_width)
        )

    popupGraph(
        plot,
        type = "png",
        width = width,
        height = height
    )
}

#' Build a riverfly species popup plot
#'
#' Plot one site's riverfly abundance observations using the abundance
#' categories shown in the map legend.
#'
#' @param row_data A data frame with `survey_date` and `abundance` columns.
#' @param breaks_vector Numeric interval breaks for the abundance categories.
#' @param date_range The date limits to display on the x-axis.
#' @param title_text Text to wrap and display as the plot title.
#' @param title_wrap_width Maximum character width for the wrapped title.
#' @param width Popup plot width in pixels.
#' @param height Popup plot height in pixels.
#' @return A Leaflet popup graph widget.
#' @keywords internal
#' @noRd
make_riverflySpecies_popup <- function(
    row_data,
    breaks_vector,
    date_range,
    title_text,
    title_wrap_width,
    width,
    height
) {
    plot <- ggplot(
        row_data,
        aes(
            x = as.Date(survey_date),
            y = abundance,
            fill = cut(
                abundance,
                breaks = breaks_vector,
                labels = c("0", "1-9", "10-99", "100-999", ">1000")
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
        ) +
        # Match the numeric y positions to the five abundance bands above.
        scale_y_continuous(
            breaks = c(0, 1, 2, 3, 4),
            labels = c("0", "1-9", "10-99", "100-999", ">1000"),
            limits = c(0, 4)
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
            ),
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        ggtitle(
            str_wrap(title_text, width = title_wrap_width)
        )

    popupGraph(
        plot,
        type = "png",
        width = width,
        height = height
    )
}
