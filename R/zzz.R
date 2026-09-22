.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    path <- system.file(
        "extdata",
        "birminghamriverchampions-db5399f61d80.json",
        package = "bhamrc"
    )

    googlesheets4::gs4_auth(
        path = path
    )

    if (!nzchar(path)) {
        stop("Could not locate 'bhamrc' package or its extdata folder")
    } else if (!file.exists(path)) {
        stop("Package found, but the credentials file is missing: ", path)
    } else {
        message("Credentials file found.")
    }

    turn_newsheet_into_db()
}
