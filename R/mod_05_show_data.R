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
      width = 3,
      data_type_input_ui(ns("data_type"), which_data_types = c(1, 2)), # To add more data types, change the vector here (add 3 for invasive species, add 5 for outfall safari)
      # Search and download buttons displayed in left column
      # Search function uses the datatable native functionality
      textInput(ns("table_search"), "Search"),
      downloadButton(ns("download_csv"), "Download CSV"),
      downloadButton(ns("download_xlsx"), "Download XLSX")
    ),
    mainPanel(
      width = 9,
      # The below styling does the following within hide-dt-search div:
      #.     Hides the search box displayed above the datatable
      #.     Sets margin, padding and min height to 0
      #.     The table headers do not wrap so all column headers are one
      #.         line. This means the table height is consistent between datasets.
      tags$style(HTML(
        "
    .hide-dt-search .dataTables_filter {
      display: none !important;
    }
    .hide-dt-search .dataTables_wrapper > .row:first-child {
      margin: 0 !important;
      padding: 0 !important;
      min-height: 0 !important;
    }
    .hide-dt-search table.dataTable th {
      white-space: nowrap;
    }
  "
      )),
      # Put submission table into hide-dt-search dive
      # so the formatting rules above apply to the datatable
      div(
        class = "hide-dt-search",
        DTOutput(ns("dt_submissions"))
      ),
      textOutput(ns("survey")),
      #textOutput(ns("table_name")),
      #downloadButton(ns("download_data"), "Download Data"),
      #DT::DTOutput(ns("entries"))
    )
  )
}

#' 05_show_data Server Functions
#' @importFrom DT renderDT DTOutput
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#' @importFrom dplyr select rename_with
#' @importFrom stringr str_replace_all str_to_sentence
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

    # Filters the data in one location
    filtered_data <- reactive({
      req(!is.null(be_result()), survey())
      be_result() |> filter(dataset == survey_map[[survey()]])
    })

    # Our initial table needs to contain _all_ of the
    # possible columns. The dataframe is created in the
    # file
    # Create the proxy
    dt_proxy <- dataTableProxy("dt_submissions")
    output$dt_submissions <- DT::renderDataTable({
      # Show data if available, otherwise blank table
      initial_data <- isolate({
        if (
          !is.null(be_result()) && nrow(be_result()) > 0 && !is.null(survey())
        ) {
          filtered_data() |>
            select(!c("Organisation")) |>
            rename_with(~ str_replace_all(.x, "_", " ") |> str_to_sentence())
        } else {
          create_blank_submission_df() |>
            select(!c("Organisation")) |>
            rename_with(~ str_replace_all(.x, "_", " ") |> str_to_sentence())
        }
      })

      # Configure data table. Search
      # pagination, n out of x text at top of table.
      # X and Y scrolling enabled.
      datatable(
        initial_data,
        width = "100%",
        options = list(
          dom = "fript",
          scrollX = TRUE,
          scrollY = "60vh",
          scrollCollapse = TRUE,
          autoWidth = TRUE
        )
      )
    })

    excluded_cols <- c(
      "Organisation",
      "long",
      "lat",
      "email_address",
      "survey_date"
    )

    # Data prepared for download
    # Data is filtered to show a subset of columns,
    #   and based on search terms in the search box.
    download_data <- reactive({
      # Only trigger if data is available and survey selected
      req(!is.null(be_result()), survey())

      # Get columns for specific dataset and only show those columns
      cols_to_show <- get_relevant_dataset_columns(survey_map[[survey()]])
      cols_to_show <- setdiff(cols_to_show, excluded_cols)

      # Filter dataframe to only show those columns
      data <- filtered_data() |>
        select(any_of(cols_to_show))

      # Get search string
      search_term <- input$table_search

      # If search_term is not empty
      if (!is.null(search_term) && nzchar(search_term)) {
        # Find rows containing search term
        match_rows <- apply(data, 1, function(row) {
          any(grepl(search_term, row, ignore.case = TRUE, fixed = FALSE))
        })

        # Get these matching rows
        data <- data[match_rows, , drop = FALSE]
      }

      # Return filtered data
      return(
        data
      )
    })

    # Download handlers which use above filtered download_data()
    output$download_csv <- downloadHandler(
      filename = function() "birmingham_river_champions_submissions.csv",
      content = function(file) {
        write.csv(download_data(), file, row.names = FALSE)
      }
    )
    output$download_xlsx <- downloadHandler(
      filename = function() "birmingham_river_champions_submissions.xlsx",
      content = function(file) {
        writexl::write_xlsx(download_data(), file)
      }
    )

    # Update data based on drop down selection
    survey_map <- c(
      riverfly = "Urban Riverfly",
      water_quality = "Water Quality",
      invasive_species = "Invasive Species",
      outfall_safari = "Urban Outfall Survey"
    )

    # Update table when either be_result or survey
    # selection changes.
    # Note: we use DT:: to use the datatable updated functions
    observeEvent(
      list(be_result(), survey()),
      {
        req(!is.null(be_result()))
        req(survey())
        DT::replaceData(
          dt_proxy,
          filtered_data() |>
            select(!c("Organisation")) |>
            # Clear up column names
            rename_with(~ str_replace_all(.x, "_", " ") |> str_to_sentence()),
          resetPaging = FALSE,
          clearSelection = "none",
        )

        # filter out the data not relevent to
        # the dataset we're looking at
        pretty_cols <- function(x) {
          x |>
            setdiff(c("Organisation", "long", "lat")) |>
            str_replace_all("_", " ") |>
            str_to_sentence()
        }

        all_cols <- names(be_result()) |>
          pretty_cols()

        cols_to_show <- get_relevant_dataset_columns(survey_map[[survey()]]) |>
          pretty_cols()
        show_cols <- which(all_cols %in% cols_to_show)
        hide_cols <- which(!all_cols %in% cols_to_show)

        #browser()

        showCols(dt_proxy, show_cols)
        hideCols(dt_proxy, hide_cols)
      }
    )

    # Update search in datatable when table_search value changes
    observeEvent(input$table_search, {
      dt_proxy |> DT::updateSearch(keywords = list(global = input$table_search))
    })

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
