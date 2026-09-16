.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    googlesheets4::gs4_auth(
        path = system.file(
            "extdata",
            "birminghamriverchampions-db5399f61d80.json",
            package = "bhamrc"
        )
    )

    turn_newsheet_into_db()
}
