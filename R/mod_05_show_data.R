#' 05_show_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_05_show_data_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "data_type",
        "Select survey:",
        choices = c(
          " ",
          "Urban Riverfly",
          "Water Quality",
          "Invasive Species",
          "Urban Outfall"
        )
      ),
      actionButton("submit_dt", "Submit", class = "btn-primary")
    ),
    mainPanel(
      h3("Submit your entry using the form."),
      DT::DTOutput("entries")
    )
  )
}

#' 05_show_data Server Functions
#'
#' @noRd
mod_05_show_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$submit_dt, {
      req(input$data_type)
      switch(
        input$data_type,
        "Urban Riverfly" = {
          data_type <- "riverfly"
        },
        "Water Quality" = {
          data_type <- "water_quality"
        },
        "Invasive Species" = {
          data_type <- "invasive_species"
        },
        "Urban Outfall" = {
          data_type <- "urban_outfall"
        }
      )
      dbExecute(
        con,
        output$entries <- DT::renderDT({
          dbGetQuery(
            con,
            paste("SELECT * FROM ", data_type, " ORDER BY id DESC")
          )
        })
      )
    })
  })
}

## To be copied in the UI
# mod_05_show_data_ui("05_show_data_1")

## To be copied in the server
# mod_05_show_data_server("05_show_data_1")
