#' extra_taxa_input
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# UI
extra_taxa_input_ui <- function(
    id,
    label
) {
    ns <- NS(id)

    shiny::tagList(
        shiny::textInput(
            ns("taxa_text"),
            label = label,
            value = ""
        ),
        shiny::radioButtons(
            ns("taxa_abundance"),
            label = label,
            choices = choices_list$abundance,
            selected = "0",
            inline = TRUE
        )
    )
}

# Server
extra_taxa_input_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        taxa_text <- reactive(input$taxa_text)
        taxa_abundance <- reactive(input$taxa_abundance)
    })
}
