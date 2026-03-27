#' @importFrom googlesheets4 gs4_auth gs4_deauth
.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # Initialize and populate db when package is loaded
    gmailr::gm_auth_configure(path = system.file("extdata","client_secret_764772056864-3am8o67h8h9dr1p8t1429ek6nfjccopc.apps.googleusercontent.com.json"))
                      #system.file("extdata","birminghamriverchampions-f263e313b398.json"))
    googlesheets4::gs4_deauth()
    turn_gsheets_into_db()
}
