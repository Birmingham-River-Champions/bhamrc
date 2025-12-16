.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # Initialize and populate db when package is loaded
    googlesheets4::gs4_auth(
        path = "./inst/extdata/birminghamriverchampions-433995c2fddf.json"
    )
    turn_gsheets_into_db()
}
