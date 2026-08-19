make_armi_popup <- function(
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
      y = ARMI,
      fill = cut(
        ARMI,
        breaks = breaks_vector,
        labels = c(brewer.pal(n = 5, name = "RdBu"))
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
    ggtitle(str_wrap(title_text, width = title_wrap_width))

  popupGraph(p, type = "png", width = width, height = height)
}
