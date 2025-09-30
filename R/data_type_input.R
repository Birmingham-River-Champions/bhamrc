#' type_input
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# UI
data_type_input_ui <- function(id) {
    ns <- NS(id)
    selectInput(
        ns("data_type"),
        "Select survey:",
        choices = c(
            "Urban Riverfly",
            "Water Quality",
            "Invasive Species"
        ),
        selected = "Urban Riverfly"
    )
}

# Server
data_type_input_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        reactive(input$data_type)
    })
}
