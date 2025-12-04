#' db_create
#'
#' @description A function to create a SQLite database with specified tables for storing Riverfly and Water Quality data.
#' @param table_name Name of the SQLite table file to create.
#' @param db_path Path to the SQLite database file.
#' @return The return value, if any, from executing the function.
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbExistsTable
#' @importFrom RSQLite SQLite
#' @noRd
db_create <- function(table_name = "riverfly", db_path = "data.sqlite") {
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
            other_acroloxidae_ancylidae TEXT, other_bullhead TEXT, other_unspecified_1 TEXT,
            other_unspecified_2 TEXT, other_unspecified_3 TEXT, other_unspecified_4 TEXT, other_unspecified_5 TEXT,
            other_unspecified_6 TEXT, other_unspecified_7 TEXT, other_unspecified_8 TEXT, names_of_other_taxa TEXT)"
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
            "(id INTEGER PRIMARY KEY, organisation TEXT, survey_date TEXT, data_type TEXT, invasive_spp_sampling_date TEXT,
            sampling_site TEXT,
            invasive_spp_wtw TEXT, signal_crayfish TEXT, killer_demon_shrimp TEXT,
            himalayan_balsam TEXT, japanese_knotweed TEXT, giant_hogweed TEXT,
            any_other_invasive_spp TEXT)"
        ),
        "outfall_safari" = paste(
            "CREATE TABLE",
            table_name,
            "(id INTEGER PRIMARY KEY, organisation TEXT, survey_date TEXT, data_type TEXT, outfall_survey_date TEXT,
            sampling_site TEXT, outfall_photo TEXT,
            outfall_flow TEXT, outfall_pollution_distance TEXT, outfall_aesthetics TEXT, other_pollution_description TEXT)"
        ),
        "riverflytest" = paste(
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
        "outfall_locs" = paste(
            "CREATE TABLE",
            table_name,
            "(id INTEGER PRIMARY KEY, Organisation TEXT, sampling_site TEXT, Easting INTEGER, Northing INTEGER,
            LAT REAL, LONG REAL)"
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
