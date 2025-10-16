#' @noRd
test_fixture_riverfly <- function() {
    # Make a copy of the data for testing
    con <- DBI::dbConnect(
        RSQLite::SQLite(),
        "data.sqlite",
        extended_types = TRUE
    )
    print(getwd())
    table_name <- c(
        "riverfly",
        "water_quality",
        "outfall_safari",
        "invasive_species",
        "riverfly_locs"
    )
    test_df <- vector("list")
    for (i in seq_len(length(table_name))) {
        test_df[[i]] <- DBI::dbReadTable(con, table_name[i])
    }
    dbDisconnect(con)

    names(test_df) <- table_name

    return(test_df)
}
