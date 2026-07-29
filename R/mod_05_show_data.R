#' 05_show_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT renderDT DTOutput dataTableProxy datatable replaceData
mod_05_show_data_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      data_type_input_ui(ns("data_type"), which_data_types = c(1, 2)) # To add more data types, change the vector here (add 3 for invasive species, add 5 for outfall safari)
    ),
    mainPanel(
      DTOutput(ns("dt_submissions")),
      #textOutput(ns("survey")),
      #textOutput(ns("table_name")),
      #downloadButton(ns("download_data"), "Download Data"),
      #DT::DTOutput(ns("entries"))
    )
  )
}

#' 05_show_data Server Functions
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom DT renderDT DTOutput
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#' @noRd
mod_05_show_data_server <- function(id, be_result) {
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

    # Datatable proxy
    # Using a proxy allows us to update
    # the table without rerendering it entirely.

    # This approach improves the user experience.
    # Our approach here is to (a) combine all the data
    # into one table in the backend and (b) show all
    # of the data in one table, then (c) filter the
    # rows and columns of the table dynamically.

    # Our initial table needs to contain _all_ of the
    # possible columns. The dataframe is created in the
    # file
    # Create the proxy
    dt_proxy <- dataTableProxy("dt_submissions")
    output$dt_submissions <- renderDataTable({
      datatable(
        create_blank_submission_df()
      )
    })

    # Update data based on drop down selection
    survey_map <- c(
      riverfly = "Urban Riverfly",
      water_quality = "Water Quality",
      invasive_species = "Invasive Species",
      outfall_safari = "Urban Outfall Survey"
    )

    # Update table when either be_result or survey
    # selection changes.
    # TODO: Currently update on first load
    observeEvent(
      list(be_result(), survey()),
      {
        req(be_result())
        req(survey())
        replaceData(
          dt_proxy,
          be_result()$data$df_geolocated_submissions |>
            filter(sheet == survey_map[[survey()]]),
          resetPaging = FALSE,
          clearSelection = "none",
        )
      }
    )

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
          arrange(desc(survey_date)) |>
          stats::setNames(column_names[[survey()]])
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
        ) |>
          select(-c(id, timestamp, email_address)) |>
          mutate(survey_date = lubridate::dmy(survey_date)) |>
          stats::setNames(column_names[[survey()]])

        writexl::write_xlsx(data_to_download, path = file)
      }
    )
  })
}

## To be copied in the UI
# mod_05_show_data_ui("05_show_data_1")

## To be copied in the server
# mod_05_show_data_server("05_show_data_1")
