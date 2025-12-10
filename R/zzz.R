.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # Initialize and populate db when package is loaded
    googlesheets4::gs4_auth(
        path = Sys.getenv("google_service_account")
    )
    turn_gsheets_into_db()
}
