get_relevant_dataset_columns <- function(dataset) {
  # Get a list of cols to show
  # Remove email, timestamp and location data from the list
  cols <- list(
    "Urban Riverfly" = riverfly_cols[
      !riverfly_cols %in% c("timestamp", "email_address", "LONG", "LAT")
    ],
    "Water Quality" = water_quality_cols[
      !water_quality_cols %in% c("timestamp", "email_address", "LONG", "LAT")
    ],
    "Invasive Species" = c(
      "timestamp"
    ),
    "Urban Outfall Survey" = c(
      "timestamp"
    )
  )

  if (dataset %in% names(cols)) {
    cols[[dataset]]
  } else {
    character(0)
  }
}
