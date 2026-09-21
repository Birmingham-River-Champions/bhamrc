.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    googlesheets4::gs4_auth(
        'inst/extdata/birminghamriverchampions-db5399f61d80.json'
    )

    turn_newsheet_into_db()
}
