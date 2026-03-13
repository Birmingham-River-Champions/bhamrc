#' @importFrom googlesheets4 gs4_auth gs4_deauth
.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # Initialize and populate db when package is loaded
    gmailr::gm_auth_configure()
    googlesheets4::gs4_deauth()
    turn_gsheets_into_db()
}
