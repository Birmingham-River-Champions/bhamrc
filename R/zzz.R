#' @importFrom googlesheets4 gs4_auth gs4_deauth
.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # Initialize and populate db when package is loaded
    #googlesheets4::gs4_auth(
    #    path = "./inst/extdata/birminghamriverchampions-36c3c5598be5.json"
    #)
    googlesheets4::gs4_deauth()
    turn_gsheets_into_db()
}
