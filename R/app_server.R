#' The application server-side
#'
#' @param input input from ui
#' @param output output for ui
#' @param session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbExistsTable dbGetQuery
#' @import RSQLite
#' @noRd
app_server <- function(input, output, session) {
  # Setup reactive for non-reactive be environment
  be_result <- reactivePoll(
    # Poll every x milliseconds
    interval = 100,
    session = session,

    # This checks the background task data
    checkFunc = function() {
      be$data
    },

    # Once there is new data in the
    # mirai job found by reactivePoll
    # this data is then assigned to
    # the be_result reactive variable
    valueFunc = function() {
      be$data
    }
  )

  mod_02_data_input_server("02_data_input_1")
  mod_03_plot_data_server("03_plot_data_1", be_result)
  mod_04_information_server("04_information_1", session)
  mod_05_show_data_server("05_show_data_1", be_result)

  table_name <- reactive(input$data_type)

  # Display selected table name
  observeEvent(input$submit_dt, {
    output$survey <- renderText(table_name())
  })
}
