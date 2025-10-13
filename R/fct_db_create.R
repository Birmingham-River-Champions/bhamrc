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
            sampling_site TEXT, cased_caddisfly TEXT, caseless_caddisfly TEXT,
            olive_mayfly TEXT, blue_winged_olive_mayfly TEXT,
            freshwater_shrimp TEXT, freshwater_hoglouse TEXT, blackfly_larvae TEXT, 
            freshwater_worm TEXT, freshwater_leech TEXT, freshwater_snail TEXT, 
            freshwater_beetle TEXT, green_drake_mayfly TEXT, flat_bodied_stone_clinger_mayfly TEXT,
            stonefly_plecoptera TEXT, other_chironomidae TEXT, other_dicranota TEXT,
            other_tipulidae TEXT, other_hydracarina TEXT, other_hydropsychidae TEXT,
            other_rhyacophilidae TEXT, other_planorbidae TEXT, other_sphaeriidae TEXT,
            other_acroloxidae_ancylidae TEXT, other_bullhead TEXT)"
        ),
        "water_quality" = paste(
            "CREATE TABLE",
            table_name,
            "(id INTEGER PRIMARY KEY, organisation TEXT, survey_date TEXT, data_type TEXT, sampling_site TEXT, 
            conductivity_mS REAL, temperature_C REAL, ammonia_ppm REAL, phosphate_ppm REAL, 
            nitrate_ppm REAL, turbidity_NTU REAL, other_water_quality TEXT)"
        ),
        "riverfly_locs" = paste(
            "CREATE TABLE",
            table_name,
            "(id INTEGER PRIMARY KEY, sampling_site TEXT, Organisation TEXT, Easting INTEGER, Northing INTEGER,
            LAT REAL, LONG REAL)"
        ),
        "invasive_species" = paste(
            "CREATE TABLE",
            table_name,
            "(id INTEGER PRIMARY KEY, organisation TEXT, survey_date TEXT, data_type TEXT, sampling_site TEXT, invasive_spp_sampling_site TEXT,
            invasive_spp_what_three_words TEXT, signal_crayfish TEXT, killer_demon_shrimp TEXT,
            himalayan_balsam TEXT, japanese_knotweed TEXT, giant_hogweed TEXT,
            any_other_invasive_spp TEXT)"
        ),
        # Default case if no match is found
        stop("Unknown table name")
    )

    # Connect to the SQLite database (or create it if it doesn't exist)
    # Create a new table if it doesn't already exist
    con <- dbConnect(RSQLite::SQLite(), "data.sqlite", extended_types = TRUE)
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
