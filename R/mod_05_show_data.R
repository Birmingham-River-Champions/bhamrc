#' 05_show_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT renderDT DTOutput dataTableProxy datatable replaceData hideCols showCols
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
        create_blank_submission_df(),
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "csv",
              text = '<i class="fa fa-download"></i> Download CSV',
              filename = "birmingham_river_champions_submissions",
              exportOptions = list(
                columns = ":visible"
              )
            ),
            list(
              extend = "excel",
              text = '<i class="fa fa-download"></i> Download XLSX',
              filename = "birmingham_river_champions_submissions",
              exportOptions = list(
                columns = ":visible"
              )
            )
          )
        )
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
        req(!is.null(be_result()))
        req(survey())
        replaceData(
          dt_proxy,
          be_result() |>
            filter(dataset == survey_map[[survey()]]),
          resetPaging = FALSE,
          clearSelection = "none",
        )

        # filter out the data not relevent to
        # the dataset we're looking at
        all_cols <- names(be_result())

        cols_to_show <- get_relevant_dataset_columns(survey_map[[survey()]])

        show_cols <- which(all_cols %in% cols_to_show)
        hide_cols <- which(!all_cols %in% cols_to_show)

        #browser()

        showCols(dt_proxy, show_cols)
        hideCols(dt_proxy, hide_cols)
      }
    )

    # We want the table to be rendered
    # when the app starts. Rather than only when
    # we click on the tab
    outputOptions(
      output,
      "dt_submissions",
      suspendWhenHidden = FALSE
    )

    # Display selected table name
    output$survey <- renderText({
      paste("Selected survey table:", survey())
    })
  })
}

## To be copied in the UI
# mod_05_show_data_ui("05_show_data_1")

## To be copied in the server
# mod_05_show_data_server("05_show_data_1")
