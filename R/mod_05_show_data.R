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
      data_type_input_ui(ns("data_type"), which_data_types = c(1, 2, 3, 4, 5))
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
        }
      )
    })
    browser()
    # Display selected table name
    output$survey <- renderText({
      paste("Selected survey table:", survey())
    })
    output$table_name <- renderText(table_name())

    output$entries <- DT::renderDT(
      {
        dbReadTable(
          con,
          survey()
        )
      },
      editable = "cell"
    )

    onStop(function() {
      dbDisconnect(con)
    })

    output$download_data <- downloadHandler(
      filename = function() {
        paste0(survey(), "_data.csv")
      },
      content = function(file) {
        data_to_download <- dbReadTable(
          con,
          survey()
        )
        write.csv(data_to_download, file, row.names = FALSE)
      }
    )

    # observeEvent(
    #   session$ns("entries"),
    #   {
    #     # Placeholder for future functionality when entries are edited

    #     con <- dbConnect(
    #       RSQLite::SQLite(),
    #       "data.sqlite",
    #       extended_types = TRUE
    #     )
    #     new_data <- dbReadTable(
    #       con,
    #       survey(),
    #       editable = TRUE
    #     )
    #     dbDisconnect(con)
    #     edited_row <- output$entries_cell_edit$row
    #     edited_col <- output$entries_cell_edit$col
    #     new_data[edited_row, edited_col] <- output$entries_cell_edit$value

    #     populate_db(new_data, "riverflytest")
    #   }
    # )
  })
}

## To be copied in the UI
# mod_05_show_data_ui("05_show_data_1")

## To be copied in the server
# mod_05_show_data_server("05_show_data_1")
