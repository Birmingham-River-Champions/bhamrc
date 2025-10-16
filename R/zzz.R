.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # Initialize and populate db when package is loaded
    turn_gsheets_into_db()
}
