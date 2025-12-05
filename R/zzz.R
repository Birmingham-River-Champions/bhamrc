.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # Initialize and populate db when package is loaded
    googlesheets4::gs4_deauth()
    turn_gsheets_into_db()
}
