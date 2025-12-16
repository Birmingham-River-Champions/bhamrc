#' @importFrom googlesheets4 gs4_auth gs4_deauth
.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # Initialize and populate db when package is loaded
    if (Sys.info()[["user"]] %in% c("stawitcc", "whitejcz")) {
        googlesheets4::gs4_auth(
            path = "./inst/extdata/birminghamriverchampions-433995c2fddf.json"
        )
    } else {
        googlesheets4::gs_deauth()
    }
    turn_gsheets_into_db()
}
