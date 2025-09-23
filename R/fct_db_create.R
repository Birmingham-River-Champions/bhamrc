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
            "(id INTEGER PRIMARY KEY, organisation TEXT, survey_date TEXT, data_type TEXT,
            sampling_site TEXT, cased_caddisfly INTEGER, caseless_caddisfly INTEGER,
            olive_mayfly INTEGER, blue_winged_olive_mayfly INTEGER,
            freshwater_shrimp INTEGER, freshwater_hoglouse INTEGER, blackfly_larvae INTEGER, 
            freshwater_worm INTEGER, freshwater_leech INTEGER, freshwater_snail INTEGER, 
            freshwater_beetle INTEGER, green_drake_mayfly INTEGER, flat_bodied_stone_clinger_mayfly INTEGER,
            stonefly_plecoptera INTEGER, other_chironomidae INTEGER, other_dicranota INTEGER,
            other_tipulidae INTEGER, other_hydracarina INTEGER, other_hydropsychidae INTEGER,
            other_rhyacophilidae INTEGER, other_planorbidae INTEGER, other_sphaeriidae INTEGER,
            other_acroloxidae_ancylidae INTEGER, other_bullhead INTEGER)"
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
            "(id INTEGER PRIMARY KEY, sampling_site TEXT, Organisation TEXT, Easting INTEGER, Northing INTEGER,
            LAT REAL, LONG REAL)"
        ),
        stop("Unknown table name")
    )

    # Connect to the SQLite database (or create it if it doesn't exist)
    # Create a new table if it doesn't already exist
    con <- dbConnect(RSQLite::SQLite(), "data.sqlite")
    if (!dbExistsTable(con, table_name)) {
        dbExecute(con, sql_string)
    } else {
        message(paste(
            "Table",
            table_name,
            "already exists. Deleting and recreating."
        ))
        dbRemoveTable(con, table_name)
        dbExecute(con, sql_string)
    }

    dbDisconnect(con)
}
