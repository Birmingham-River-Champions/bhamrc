#' 04_information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom yaml read_yaml
#' @importFrom purrr map
mod_04_information_ui <- function(id) {
  ns <- NS(id)

  information_data <- yaml::read_yaml(
    app_sys("app/www/text/information.yml")
  )$information

  tagList(
    # Main Title (centered)
    h1(id = "main-title", HTML("<b>Key information</b>")), # Using HTML() for bold text
    # Grid layout for left (summary) and right (news post)
    div(
      id = "project-grid",
      purrr::map(
        information_data,
        ~ information_card(
          title = .x$title,
          description = .x$description,
          url = .x$url,
          image_src = .x$image_src,
          ns = ns
        )
      )
    )
  )
}

#' 04_information Server Functions
#'
#' @noRd
mod_04_information_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input[["Survey form"]], {
      updateTabsetPanel(parent_session, "panels", selected = "submission_form")
    })
  })
}

## To be copied in the UI
# mod_04_information_ui("04_information_1")

## To be copied in the server
# mod_04_information_server("04_information_1")
