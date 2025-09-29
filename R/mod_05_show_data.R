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
        ns("data_type"),
        "Select survey:",
        choices = c(
          "Urban Riverfly",
          "Water Quality",
          "Invasive Species",
          "Urban Outfall Safari"
        ),
        selected = "Urban Riverfly"
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

    table_name <- reactive({
      survey <- input$data_type
      switch(
        survey,
        "Urban Riverfly" = {
          table_name <- "riverfly"
        },
        "Water Quality" = {
          table_name <- "water_quality"
        },
        "Invasive Species" = {
          table_name <- "invasive_species"
        },
        "Urban Outfall Safari" = {
          table_name <- "urban_outfall"
        }
      )
    })
    con <- dbConnect(RSQLite::SQLite(), "data/database.sqlite")
    output$entries <- DT::renderDT({
      dbGetQuery(
        con,
        paste("SELECT * FROM ", table_name, " ORDER BY id DESC")
      )
    })
    dbDisconnect(con)
  })
}

## To be copied in the UI
# mod_05_show_data_ui("05_show_data_1")

## To be copied in the server
# mod_05_show_data_server("05_show_data_1")
