.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    googlesheets4::gs4_auth(
        path = system.file(
            "extdata",
            "birminghamriverchampions-db5399f61d80.json",
            package = "bhamrc"
        )
    )

    path <- system.file(
        "extdata",
        "birminghamriverchampions-db5399f61d80.json",
        package = "bhamrc"
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
