.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # If running locally
    googlesheets4::gs4_auth(
        path = "./inst/extdata/birminghamriverchampions-db5399f61d80.json"
    )

    # If running on github repo
    cred_file <- "inst/extdata/birminghamriverchampions-db5399f61d80.json"

    # Verify credential is created
    message("cred_file = ", cred_file)
    message("exists = ", file.exists(cred_file))

    turn_newsheet_into_db()
}
