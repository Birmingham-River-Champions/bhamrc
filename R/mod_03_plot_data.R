#' 03_plot_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput
mod_03_plot_data_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      selectInput(
        ns("metric"),
        "Select the survey from the drop down menu:",
        choices = c(
          " ",
          "Urban Riverfly",
          "Invasive Species",
          "Water Chemistry"
        )
      ),
      conditionalPanel(
        condition = "input.metric == 'Urban Riverfly'",
        selectInput(
          ns("riverfly"),
          "Choose:",
          choices = c(
            " ",
            "ARMI",
            "Urban Riverfly species",
            "Other species"
          )
        ),
        conditionalPanel(
          condition = "input.riverfly == 'ARMI'&& input.metric == 'Urban Riverfly'",
          includeMarkdown(app_sys("app/www/text/ARMI_description.md")),
          ns = ns
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.riverfly == 'Urban Riverfly species'&& input.metric == 'Urban Riverfly'",
        radioButtons(
          ns("riverflySpecies"),
          "Urban Riverfly species",
          choices = unname(unlist(riverfly_spp_bw))
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.riverfly == 'Other species'&& input.metric == 'Urban Riverfly'",
        radioButtons(
          ns("otherSpecies"),
          "Other species",
          choices = unname(unlist(other_spp_bw))
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.metric == 'Water Chemistry'",
        radioButtons(
          ns("readingType"),
          "Choose water chemistry reading type:",
          choices = c(
            "Conductivity (\u03BCS)" = "conductivity_mS",
            "Temperature (\u00B0C)" = "temperature_C",
            "Ammonia (ppm)" = "ammonia_ppm",
            "Phosphate (ppm)" = "phosphate_ppm",
            "Nitrate (ppm)" = "nitrate_ppm",
            "Turbidity (NTU)" = "turbidity_NTU"
          )
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.metric == 'Invasive Species'",
        radioButtons(
          ns("invasiveType"),
          "Choose invasive species:",
          choices = c(
            "Signal crayfish" = "signal_crayfish",
            "Killer or demon shrimp" = "killer_demon_shrimp",
            "Himalayan balsam" = "himalayan_balsam",
            "Giant hogweed" = "giant_hogweed",
            "Japanese knotweed" = "japanese_knotweed"
          )
        ),
        ns = ns
      ),
    ),
    mainPanel(
      #leafletOutput(ns('submission_map')),
      div(
        id = "yourdata-descriptor",
        HTML(
          "<b>Select the survey from the drop down menus and click on each point to view extra details.</b> Points on the map may take a few seconds to load."
        )
      ),
      # Map: Use a separate class for the Leaflet map
      div(
        class = "leaflet-map-container",
        leaflet::leafletOutput(ns("submission_map"))
      ),

      # ggplot output: Use a separate class for the ggplot popups
      div(
        class = "ggplot-container",
        plotOutput(ns("ggplot"))
      )
    )
  )
}

#' 03_plot_data Server Functions
#' @importFrom leaflet leafletProxy addProviderTiles setView clearMarkers addCircleMarkers addLegend clearControls showGroup hideGroup
#' @importFrom leaflet providers leafletOptions renderLeaflet leaflet addLayersControl layersControlOptions
#' @importFrom dplyr filter mutate rowwise
#' @noRd
mod_03_plot_data_server <- function(id, be_result) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Initialize the Leaflet map
    output$submission_map <- renderLeaflet({
      leaflet(
        options = leafletOptions(
          zoomControl = FALSE,
          attributionControl = FALSE
        )
      ) |>
        htmlwidgets::onRender(
          "function(el, x) {
          L.control.zoom({ position: 'bottomright' }).addTo(this)
      }"
        ) |>
        addProviderTiles(providers$OpenStreetMap) |>
        setView(lng = -1.83, lat = 52.45, zoom = 10) |>
        addPolygonsAndLines(zoomLevel = 10) # Add polygons and lines at initial zoom level
    })

    # Reactive expressions to capture user selections
    selected_metric <- reactive(input$metric)
    selected_riverfly <- reactive(input$riverfly)
    selected_riverfly_species <- reactive(input$riverflySpecies)
    selected_other_species <- reactive(input$otherSpecies)
    selected_invasive_type <- reactive(input$invasiveType)
    selected_reading_type <- reactive(input$readingType)
    screen_width <- reactive(input$screen_width)

    # Update the map with appropriate data
    updateMap <- function(input, output, session) {
      selected_dataset <- selected_metric()
      if (selected_dataset == "Water Chemistry") {
        selected_dataset <- "Water Quality"
      }

      # Picks if the user selects Water Quality, Urban Riverfly or Invasive Species
      plot_data <- be_result()$df_geolocated_submissions |>
        filter(dataset == selected_dataset)

      # Map proxy which is updated depending on selection
      mapProxy <- leafletProxy("submission_map")

      mapProxy |>
        clearMapLayers()

      ## Urban riverfly
      if (selected_metric() == "Urban Riverfly") {
        if (selected_riverfly() == "ARMI") {
          # Plot ARMI data
          mapProxy |>
            addARMIMarkers(
              map_data = be_result()$riverflyARMIMap,
              popup_data = be_result()$Riverfly_ARMI_Popups,
              screen_width = screen_width()
            ) |>
            showGroup("ARMI points")
        } else if (selected_riverfly() == "Urban Riverfly species") {
          # Plot urban riverfly species data
          # If the user chooses Urban Riverfly species, plot abundance data
          # Filter by the selected Taxa
          selectedTaxa <- names(which(
            riverfly_spp_bw == selected_riverfly_species()
          ))
          riverfly_species_popups <- be_result()$Riverfly_Species_Plot[grepl(
            selectedTaxa,
            names(be_result()$Riverfly_Species_Plot)
          )]
          mapProxy |>
            addRiverflySpeciesMarkers(
              popup_data = be_result()$riverfly_species_popups,
              map_data = be_result()$Riverfly_Species_Plot_Recent[[
                selectedTaxa
              ]],
              selectedTaxa,
              screen_width()
            ) |>
            showGroup("Riverfly points")
        } else if (selected_riverfly() == "Other species") {
          # Plot other riverfly species data
        } else if (selected_metric() == "Invasive Species") {
          # Plot invasiv species
        } else if (selected_metric() == "Water Chemistry") {
          # Plot qater qualiry data
        }
      }
    }

    observeEvent(
      {
        list(
          input$metric,
          input$readingType,
          input$invasiveType,
          input$riverfly,
          input$riverflySpecies,
          input$otherSpecies,
          be_result()$df_geolocated_submissions
        )
      },
      {
        req(be_result()$df_geolocated_submissions)
        updateMap(input, output, session)
      },
      ignoreInit = TRUE
    )

    # We want the plot to be rendered
    # when the app starts. Rather than only when
    # we click on the tab
    outputOptions(
      output,
      "submission_map",
      suspendWhenHidden = FALSE
    )
  })
}

## To be copied in the UI
# mod_03_plot_data_ui("03_plot_data_1")

## To be copied in the server
# mod_03_plot_data_server("03_plot_data_1")
