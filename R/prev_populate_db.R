#' populate_db
#'
#' @description A fct function
#' @param data_to_insert A data frame containing the data to be inserted into the database.
#' @param table_name Name of the SQLite table where data should be inserted.
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#'
#' @return The return value, if any, from executing the function.
populate_db <- function(data_to_insert, table_name) {
    # Connect to the SQLite database
    con <- dbConnect(RSQLite::SQLite(), "data.sqlite", extended_types = TRUE)

    # Insert data into the table_name table
    # Assuming `data_to_insert` is a data frame with the appropriate columns
    dbAppendTable(
        conn = con,
        name = table_name,
        value = data.frame(data_to_insert)
    )

    # Disconnect from the database
    dbDisconnect(con)
}
