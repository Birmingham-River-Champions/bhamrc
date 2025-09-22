#' 02_data_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_data_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput("name", "Name"),
        textInput("email", "Email"),
        textAreaInput("comment", "Comment"),
        actionButton("submit", "Submit", class = "btn-primary")
      ),
      mainPanel(
        h3("Submit your entry using the form.")
      )
    )
  )
}

#' 02_data_input Server Functions
#'
#' @noRd
mod_02_data_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_02_data_input_ui("02_data_input_1")

## To be copied in the server
# mod_02_data_input_server("02_data_input_1")
