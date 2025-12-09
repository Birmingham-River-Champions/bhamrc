#' Function to get the tables from the new Google Sheet
#' @param sheet_url The URL of the Google Sheet containing the data.
#' @return A list of data frames, each corresponding to a table in the Google Sheet.
#' @importFrom googlesheets4 read_sheet
#' @export
put_table_data <- function(
    sheet_url,
    data_types = c(
        "riverfly",
        "water_quality",
        "outfall_safari",
        "invasive_species"
    )
) {
    for (dt in data_types) {
        table_name <- names(data_types_bw)[data_types_bw == dt]
        each_tbl <- googlesheets4::read_sheet(sheet_url, table_name)
        # Create SQLite tables for riverfly, water quality, and associated location identifiers
        # Invasive species and outfall safari data not currently being added to the database because they are empty
        db_create(dt)

        # Populate the database tables with the cleaned data
        populate_db(each_tbl, dt)
    }

    return(table_list)
}
