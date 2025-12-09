.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # Initialize and populate db when package is loaded
    if (!rlang::is_interactive()) {
        googlesheets4::gs4_deauth()
    }
    turn_gsheets_into_db()
}
