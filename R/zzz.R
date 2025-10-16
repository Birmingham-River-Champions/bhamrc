.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")
    # Preload locations data when the package is loaded
    ns = topenv()
    ns$brc_locations_file <- system.file(
        "extdata/BRC_Sampling_Locations.csv",
        package = "bhamrc"
    )
    ns$outfall_locations_file <- system.file(
        "extdata/BRC_Outfall_Locations.csv",
        package = "bhamrc"
    )

    # Initialize and populate db when package is loaded
    turn_gsheets_into_db()
}
