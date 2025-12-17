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
  mod_02_data_input_server("02_data_input_1")
  mod_03_plot_data_server("03_plot_data_1")
  mod_04_information_server("04_information_1")
  mod_05_show_data_server("05_show_data_1")

  table_name <- reactive(input$data_type)

  # Display selected table name
  observeEvent(input$submit_dt, {
    output$survey <- renderText(table_name())
  })

  observeEvent(input$link_to_accessibility_statement, {
    updateTabsetPanel(session, "panels", selected = "accessibility")
  })
}
