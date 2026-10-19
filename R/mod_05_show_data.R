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
      width = 3,
      data_type_input_ui(ns("data_type"), which_data_types = c(1, 2)),
      textInput(ns("table_search"), "Search"),
      downloadButton(ns("download_csv"), "Download CSV"),
      downloadButton(ns("download_xlsx"), "Download XLSX")
    ),
    mainPanel(
      width = 9,
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
      div(
        class = "hide-dt-search",
        DT::DTOutput(ns("entries"))
      ),
      textOutput(ns("table_name"))
    )
  )
}

# Give each survey's database columns their configured display labels, with a
# readable fallback for fields not yet listed in the display-name config.
format_survey_table <- function(data, table) {
  data <- data |>
    dplyr::select(
      -c(
        id,
        timestamp,
        email_address
      )
    ) |>
    dplyr::mutate(
      survey_date = lubridate::dmy(survey_date)
    ) |>
    dplyr::arrange(dplyr::desc(survey_date))

  labels <- vapply(
    names(data),
    function(column) {
      table_labels <- column_names[[table]]
      label_index <- match(column, names(table_labels))
      label <- if (is.na(label_index)) NULL else table_labels[[label_index]]
      if (is.null(label) && column == "participant_number") {
        # The display-name config uses this legacy name for the same field.
        label_index <- match("number_participants", names(table_labels))
        if (!is.na(label_index)) {
          label <- table_labels[[label_index]]
        }
      }
      if (is.null(label)) {
        label <- stringr::str_to_sentence(
          stringr::str_replace_all(column, "_", " ")
        )
      }
      as.character(label)
    },
    character(1)
  )
  names(data) <- labels
  data$.survey_table <- rep(table, nrow(data))
  data
}

# Apply the same literal search used by the table to downloads so exported rows
# match the current survey and search field.

# Apply the same literal search used by the table to downloads so exported rows
# match the current survey and search field.
filter_table_search <- function(data, search_term) {
  if (is.null(search_term) || !nzchar(search_term) || nrow(data) == 0L) {
    return(data)
  }

  matching_rows <- rep(FALSE, nrow(data))
  for (column in data) {
    values <- as.character(column)
    matching_rows <- matching_rows |
      (!is.na(values) &
        stringr::str_detect(
          values,
          stringr::fixed(search_term, ignore_case = TRUE)
        ))
  }
  data[matching_rows, , drop = FALSE]
}

#' 05_show_data Server Functions
#' @importFrom DBI dbConnect dbDisconnect dbReadTable
#' @importFrom RSQLite SQLite
#' @importFrom DT renderDT DTOutput
#' @importFrom writexl write_xlsx
#' @noRd
mod_05_show_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Determine table name based on selected survey
    table_name <- data_type_input_server("data_type")
    con <- dbConnect(RSQLite::SQLite(), "data.sqlite", extended_types = TRUE)
    survey <- reactive({
      switch(
        table_name(),
        "Urban Riverfly" = {
          "riverfly"
        },
        "Water Chemistry" = {
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

    # Load the available surveys once and align them to a common set of
    # display columns. A stable schema lets the DataTable proxy replace rows
    # and toggle survey-specific columns without rebuilding the widget.
    survey_tables <- c(
      "riverfly",
      "water_quality",
      "invasive_species",
      "outfall_safari"
    )
    survey_data <- stats::setNames(
      lapply(survey_tables, function(table) {
        format_survey_table(dbReadTable(con, table), table)
      }),
      survey_tables
    )
    all_columns <- unique(unlist(lapply(
      survey_data,
      function(data) setdiff(names(data), ".survey_table")
    )))
    all_data <- dplyr::bind_rows(survey_data)

    table_data <- reactive({
      req(survey())
      all_data |>
        dplyr::filter(.survey_table == survey()) |>
        dplyr::select(-.survey_table) |>
        dplyr::select(dplyr::all_of(all_columns))
    })
    download_data <- reactive({
      data <- table_data() |>
        dplyr::select(dplyr::all_of(visible_columns()))
      filter_table_search(data, input$table_search)
    })
    visible_columns <- reactive({
      req(survey())
      names(survey_data[[survey()]]) |>
        setdiff(".survey_table")
    })

    table_proxy <- DT::dataTableProxy("entries", session = session)
    # The selector defaults to Urban Riverfly in the UI; its browser input
    # may not be available yet while the server module is being initialized.
    initial_survey <- "riverfly"
    initially_hidden <- which(
      !all_columns %in% names(survey_data[[initial_survey]])
    ) -
      1L
    output$entries <- DT::renderDT({
      options <- list(
        dom = "fript",
        scrollX = TRUE,
        scrollY = "60vh",
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        search = list(
          regex = FALSE,
          smart = FALSE,
          caseInsensitive = TRUE
        )
      )
      if (length(initially_hidden) > 0L) {
        options$columnDefs <- list(list(
          targets = initially_hidden,
          visible = FALSE
        ))
      }
      DT::datatable(
        isolate(table_data()),
        width = "100%",
        options = options
      )
    })

    # Reuse the existing widget as the selection changes. Keep the current
    # page where possible, and show only columns relevant to that survey.
    observeEvent(
      survey(),
      {
        DT::replaceData(
          table_proxy,
          table_data(),
          resetPaging = FALSE,
          clearSelection = "none"
        )
        shown <- which(all_columns %in% visible_columns())
        hidden <- which(!all_columns %in% visible_columns())
        DT::showCols(table_proxy, shown)
        DT::hideCols(table_proxy, hidden)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$table_search,
      {
        DT::updateSearch(
          table_proxy,
          keywords = list(global = input$table_search)
        )
      },
      ignoreInit = TRUE
    )

    outputOptions(output, "entries", suspendWhenHidden = FALSE)
    output$table_name <- renderText(table_name())

    onStop(function() {
      dbDisconnect(con)
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0(survey(), "_data.csv")
      },
      content = function(file) {
        utils::write.csv(download_data(), file, row.names = FALSE)
      }
    )
    output$download_xlsx <- downloadHandler(
      filename = function() {
        paste0(survey(), "_data.xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(download_data(), path = file)
      }
    )
  })
}

## To be copied in the UI
# mod_05_show_data_ui("05_show_data_1")

## To be copied in the server
# mod_05_show_data_server("05_show_data_1")
