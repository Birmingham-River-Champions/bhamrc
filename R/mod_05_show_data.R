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
      data_type_input_ui(ns("data_type"), which_data_types = c(1, 2)) # To add more data types, change the vector here (add 3 for invasive species, add 5 for outfall safari)
    ),
    mainPanel(
      textOutput(ns("survey")),
      textOutput(ns("table_name")),
      downloadButton(ns("download_data"), "Download Data"),
      DT::DTOutput(ns("entries"))
    )
  )
}

#' 05_show_data Server Functions
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom DT renderDT DTOutput
#' @importFrom writexl write_xlsx
#' @noRd
mod_05_show_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Determine table name based on selected survey
    table_name <- data_type_input_server("data_type")
    con <- dbConnect(RSQLite::SQLite(), "data.sqlite", extended_types = TRUE)
    survey <- reactive({
      switch(
        table_name(),
        "Urban Riverfly" = {
          "riverfly"
        },
        "Water Quality" = {
          "water_quality"
        },
        "Invasive Species" = {
          "invasive_species"
        },
        "Urban Outfall Safari" = {
          "outfall_safari"
        }
      )
    })

    # Display selected table name
    output$survey <- renderText({
      paste("Selected survey table:", survey())
    })
    output$table_name <- renderText(table_name())
    # Render the table from the SQL database
    output$entries <- DT::renderDT(
      {
        # Retrieve data based on chosen survey
        # Remove id column and convert date columns before displaying
        # Fix column names for display
        dbReadTable(
          con,
          survey()
        ) |>
          select(-id, -timestamp, -email_address) |>
          mutate(survey_date = lubridate::dmy(survey_date)) |>
          setNames(column_names[[survey()]])
      }
    )

    onStop(function() {
      dbDisconnect(con)
    })

    # Create download handler to download the data when clicked
    output$download_data <- downloadHandler(
      filename = function() {
        paste0(survey(), "_data.xlsx")
      },
      content = function(file) {
        data_to_download <- dbReadTable(
          con,
          survey()
        )
        writexl::write_xlsx(data_to_download, path = file)
      }
    )
  })
}

## To be copied in the UI
# mod_05_show_data_ui("05_show_data_1")

## To be copied in the server
# mod_05_show_data_server("05_show_data_1")
