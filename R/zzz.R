.onLoad <- function(libname, pkgname) {
    options(bhamrc.dbname = "data.sqlite")

    # Check if system.file reference is present
    # if not then load from relative path
    json_path <- system.file(
        "extdata",
        "birminghamriverchampions-db5399f61d80.json",
        package = pkgname
    )

    if (!nzchar(json_path)) {
        json_path <- "inst/extdata/birminghamriverchampions-db5399f61d80.json"
    }

    if (file.exists(json_path)) {
        googlesheets4::gs4_auth(json_path)
    }

    turn_newsheet_into_db()
}
