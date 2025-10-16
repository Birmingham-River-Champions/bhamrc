.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")
    turn_gsheets_into_db()
}
