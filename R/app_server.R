#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbExistsTable dbGetQuery
#' @import RSQLite
#' @noRd
app_server <- function(input, output, session) {
  # Application server logic
  observeEvent(input$submit, {
    req(input$name, input$email, input$comment)
    dbExecute(
      con,
      "INSERT INTO submissions (name, email, comment) VALUES (?, ?, ?)",
      params = list(input$name, input$email, input$comment)
    )
    showNotification("Submission successful!", type = "message")
  })

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
      "SELECT name, email, comment FROM submissions ORDER BY id DESC"
    )
  })
}
