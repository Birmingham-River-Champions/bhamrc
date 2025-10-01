#' 06_newsletters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_06_newsletters_ui <- function(id) {
  ns <- NS(id)
  newsletter_data <- yaml::read_yaml(
    app_sys("app/www/text/newsletters.yml")
  )$newsletters

  tagList(
    # Grid layout for left (summary) and right (news post)
    div(
      id = "newsletters",
      class = "project-grid",
      purrr::map(
        newsletter_data,
        ~ newsletter_card(
          title = .x$title,
          description = .x$description,
          url = .x$url,
          image_src = .x$image_src
        )
      )
    )
  )
}

#' 06_newsletters Server Functions
#'
#' @noRd
mod_06_newsletters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_06_newsletters_ui("06_newsletters_1")

## To be copied in the server
# mod_06_newsletters_server("06_newsletters_1")
