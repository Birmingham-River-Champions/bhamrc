create_armi_plots <- function(riverfly_armi_assignment) {
  # Do site split once for efficiency
  sites <- split(
    riverfly_armi_assignment,
    riverfly_armi_assignment$sampling_site
  )

  # Go through each site
  # Create plot of ARMI values
  riverfly_plots <- list()
  for (site in unique(riverfly_armi_assignment$sampling_site)) {
    df <- sites[[site]]

    organisation <- df$organisation[1]

    # Some organisations don't sound right with "the" in front
    organisation <- if (
      organisation != "Hall Green's Keepin' It Clean" &
        organisation != "Birmingham Conservation Society"
    ) {
      organisation <- paste("the", organisation)
    } else {
      organisation <- organisation # This line is optional, just for clarity
    }

    # Convert dates to date object
    # TODO: Verify date conversion works correctly
    df$survey_date <- dmy(df$survey_date)

    # Calculate date range buffer if there's only one sample
    date_range <- range(
      df$survey_date,
      na.rm = TRUE
    )
    if (diff(date_range) == 0) {
      date_range <- c(date_range[1] - 15, date_range[2] + 15)
    }

    # Set character width for str_wrap based on popup width
    # title_wrap_width <- ifelse(
    #   popup_width <= 300,
    #   37,
    #   ifelse(popup_width <= 450, 50, 75)
    # )
    title_text <- paste0(
      "ARMI score at ",
      site,
      ". Sampled by ",
      organisation,
      "."
    )

    p <- ggplot(
      df,
      aes(
        x = as.Date(survey_date),
        y = ARMI,
        # fill = cut(
        #   ARMI,
        #   breaks = breaks_vector,
        #   labels = c(brewer.pal(n = 5, name = "RdBu"))
        # )
      )
    ) +
      geom_point(size = 5, pch = 21, colour = "black") +
      theme_minimal() +
      # scale_fill_manual(
      #   name = "ARMI",
      #   values = brewer.pal(n = 6, name = "RdBu"),
      #   drop = FALSE
      # ) +
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
      ) + #+ ##Did have the text over the y-axis title, but changed to centre - ","
      ggtitle(title_text)

    # Add to list
    riverfly_plots[[site]] <- p
  }

  return(
    riverfly_plots
  )
}
