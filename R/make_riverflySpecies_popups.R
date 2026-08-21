make_riverflySpecies_popup <- function(
  row_data,
  breaks_vector,
  date_range,
  title_text,
  title_wrap_width,
  width = 300,
  height = 300
) {
  p <- ggplot(
    row_data,
    aes(
      x = as.Date(survey_date),
      y = abundance, #ARMabundanceI,
      fill = cut(
        abundance,
        breaks = breaks_vector,
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
    ) +
    scale_y_continuous(
      breaks = c(0, 1, 2, 3, 4), # Custom breaks for y-axis
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
    ggtitle(str_wrap(title_text, width = title_wrap_width))

  popupGraph(
    p,
    type = "png",
    width = width,
    height = height
  )
}
