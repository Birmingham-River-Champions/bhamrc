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
      data_type_input_ui(ns("data_type"), which_data_types = c(1, 2, 3, 5))
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
          select(-id) |>
          mutate(survey_date = lubridate::dmy(survey_date)) |>
          setNames(column_names[[survey()]])
      },
      editable = "cell"
    )

    onStop(function() {
      dbDisconnect(con)
    })

    # Create download handler to download the data when clicked
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

    # Watch for when the cells are edited
    observeEvent(
      input[[ns("entries_cell_clicked")]],
      ignoreInit = TRUE,
      {
        # Read the data from the database, update the edited cell, and write it back to the database
        output <- input[["mod_05_show_data_1-entries_cell_edit"]]
        con <- dbConnect(
          RSQLite::SQLite(),
          "data.sqlite",
          extended_types = TRUE
        )
        new_data <- dbReadTable(
          con,
          survey(),
          editable = TRUE
        )
        edited_row <- output$entries_cell_edit$row
        edited_col <- output$entries_cell_edit$col
        new_data[edited_row, edited_col] <- output$entries_cell_edit$value

        #populate_db(new_data, "riverflytest")
      }
    )
  })
}

## To be copied in the UI
# mod_05_show_data_ui("05_show_data_1")

## To be copied in the server
# mod_05_show_data_server("05_show_data_1")
