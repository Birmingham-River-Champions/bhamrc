#' type_input
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# UI
data_type_input_ui <- function(
    id,
    question_string = "Select survey",
    which_data_types = c(1, 2, 3)
) {
    ns <- NS(id)
    selectInput(
        ns("data_type"),
        question_string,
        choices = names(data_types_bw)[which_data_types],
        selected = "Urban Riverfly"
    )
}

# Server
data_type_input_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        reactive(input$data_type)
    })
}
