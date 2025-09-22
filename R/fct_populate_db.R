#' populate_db
#'
#' @description A fct function
#' @param data_to_insert A data frame containing the data to be inserted into the database.
#' @param table_name Name of the SQLite table where data should be inserted.
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
populate_db <- function(data_to_insert, table_name) {
    # Connect to the SQLite database
    con <- dbConnect(RSQLite::SQLite(), "data.sqlite")

    # Insert data into the table_name table
    # Assuming `data_to_insert` is a data frame with the appropriate columns
    dbWriteTable(
        con,
        table_name,
        data_to_insert,
        append = TRUE,
        row.names = FALSE
    )

    # Disconnect from the database
    dbDisconnect(con)
}
