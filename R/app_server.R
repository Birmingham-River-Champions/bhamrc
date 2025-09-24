#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbExistsTable dbGetQuery
#' @import RSQLite
#' @noRd
app_server <- function(input, output, session) {
  callModule(mod_02_data_input_server, "02_data_input_1")
  # Application server logic
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

  output$entries <- DT::renderDT({
    dbGetQuery(
      con,
      "SELECT * FROM riverfly ORDER BY id DESC"
    )
  })
}
