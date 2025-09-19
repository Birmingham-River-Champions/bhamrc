library(shiny)
library(DBI)
library(RSQLite)

# Database setup
con <- dbConnect(RSQLite::SQLite(), "data.sqlite")
if (!dbExistsTable(con, "submissions")) {
    dbExecute(
        con,
        "CREATE TABLE submissions (id INTEGER PRIMARY KEY, name TEXT, email TEXT, comment TEXT)"
    )
}

onStop(function() {
    dbDisconnect(con)
})

shinyApp(ui, server)
