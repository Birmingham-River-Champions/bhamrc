.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # Initialize and populate db when package is loaded
    googlesheets4::gs4_auth(
        path = ".secrets/birminghamriverchampions-1b4a4b469009.json",
    )
    turn_gsheets_into_db()
}
