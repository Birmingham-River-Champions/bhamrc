.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # If running on github repo
    cred_file <- "inst/extdata/birminghamriverchampions-db5399f61d80.json"

    message("exists = ", file.exists(cred_file))
    message("size = ", file.info(cred_file)$size)
    cat(readLines(cred_file, n = 3), sep = "\n")

    # If running locally
    googlesheets4::gs4_auth(
        path = "./inst/extdata/birminghamriverchampions-db5399f61d80.json"
    )

    turn_newsheet_into_db()
}
