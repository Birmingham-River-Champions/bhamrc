#' db_create
#'
#' @description A function to create a SQLite database with specified tables for storing Riverfly and Water Quality data.
#' @param table_name Name of the SQLite table file to create.
#' @return The return value, if any, from executing the function.
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbExistsTable
#' @importFrom RSQLite SQLite
#' @noRd
db_create <- function(table_name = "riverfly") {
    # Create a unique table for each data type
    sql_string <- switch(
        table_name,
        "riverfly" = paste(
            "CREATE TABLE",
            table_name,
            "(id INTEGER PRIMARY KEY, organisation TEXT, survey_date TEXT, sampling_site TEXT, 
            caddis_larvae TEXT, mayfly_larvae INTEGER, stonefly_larvae INTEGER, gammarus INTEGER, 
            freshwater_shrimp INTEGER, flatworm INTEGER, leech INTEGER, oligochaete_worms INTEGER, 
            chironomid_larvae INTEGER, other_benthic_inverts INTEGER, other_bullhead INTEGER)"
        ),
        "water_quality" = paste(
            "CREATE TABLE",
            table_name,
            "(id INTEGER PRIMARY KEY, organisation TEXT, survey_date TEXT, sampling_site TEXT, 
            conductivity_mS REAL, temperature_C REAL, ammonia_ppm REAL, phosphate_ppm REAL, 
            nitrate_ppm REAL, turbidity_NTU REAL, other_water_quality TEXT)"
        ),
        "riverfly_locs" = paste(
            "CREATE TABLE",
            table_name,
            "(id INTEGER PRIMARY KEY, site_id TEXT, location_name TEXT, latitude REAL, longitude REAL)"
        ),
        stop("Unknown table name")
    )

    # Connect to the SQLite database (or create it if it doesn't exist)
    # Create a new table if it doesn't already exist
    con <- dbConnect(RSQLite::SQLite(), "data.sqlite")
    if (!dbExistsTable(con, table_name)) {
        dbExecute(con, sql_string)
    }

    dbDisconnect(con)
}
