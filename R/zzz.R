.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # If running locally
    # googlesheets4::gs4_auth(
    #     path = "./inst/extdata/birminghamriverchampions-db5399f61d80.json"
    # )

    # If running on github repo
    cred_file <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")

    if (nzchar(cred_file) && file.exists(cred_file)) {
        googlesheets4::gs4_auth(path = cred_file)
    } else {
        googlesheets4::gs4_deauth()
    }

    turn_newsheet_into_db()
}
