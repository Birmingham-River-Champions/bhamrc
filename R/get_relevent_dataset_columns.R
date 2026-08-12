get_relevant_dataset_columns <- function(dataset) {
  cols <- list(
    "Urban Riverfly" = c(
      "timestamp",
      "email_address",
      "organisation"
    ),
    "Water Quality" = c(
      "organisation"
    ),
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
