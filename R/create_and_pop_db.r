# Read in data from Google Sheets
#' db_create
#' A function to create a SQLite database with specified tables for storing Riverfly and Water Quality data.
#' @param sub_table The subsetted data frame read in from the Google Sheet.
#' @param index_of_site_col The index of the column in the cleaned data that contains the sampling site information.
#' @param table_name Name of the SQLite table file to create.
#' @param ... Additional arguments to pass to the db_create function.
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr case_when
db_create_and_pop <- function(
    sub_table,
    index_of_site_col,
    table_name,
    ...
) {
    names(sub_table)[index_of_site_col] <- "sampling_site"

    # Create SQLite tables for riverfly, water quality, and associated location identifiers
    # Invasive species and outfall safari data not currently being added to the database because they are empty
    db_create(table_name, ...)

    # Populate the database tables with the cleaned data
    populate_db(sub_table, table_name)

    # Put the created data into Google Sheets as well
    # googlesheets4::sheet_write(
    #     ss = google_sheet_id,
    #     data = processed_data,
    #     sheet = data_type
    # )
}
