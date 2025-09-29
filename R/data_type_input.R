#' type_input
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# UI
ui <- function(id) {
    ns <- NS(id)
    selectInput(
        ns("data_type"),
        "Select survey:",
        choices = c(
            "Urban Riverfly",
            "Water Quality",
            "Invasive Species",
            "Urban Outfall Safari"
        ),
        selected = "Urban Riverfly"
    )
}

# Server
server <- function(id) {
    moduleServer(id, function(input, output, session) {
        reactive(input$data_type)
    })
}
