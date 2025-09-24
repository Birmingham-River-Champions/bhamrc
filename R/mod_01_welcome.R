#' 01_welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_welcome_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      list_to_p(
        list(
          includeMarkdown(app_sys("app/www/text/welcome.md")),
          HTML(
            "Please email <b> birminghamriverchampions@gmail.com </b> if you have any questions 
    or would like more information on any aspects of the project"
          )
        ),
        class = "welcome-text"
      )
    )
  )
}

#' 01_welcome Server Functions
#'
#' @noRd
mod_01_welcome_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_01_welcome_ui("01_welcome_1")

## To be copied in the server
# mod_01_welcome_server("01_welcome_1")
