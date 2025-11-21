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
            choices = abundance_choices,
            selected = "0",
            inline = TRUE
        ),
        shiny::actionButton(
            ns("add_taxa"),
            label = "Add another taxa observation",
            class = "btn-primary"
        )
    )
}

# Server
extra_taxa_input_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        taxa_name <- reactive(input$taxa_text)
        taxa_abundance <- reactive(input$taxa_abundance)
        taxa_button <- reactive(input$add_taxa)
    })
}
