#' 03_plot_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList leafletOutput
mod_03_plot_data_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      selectInput(
        ns("metric"),
        "Select survey:",
        choices = c(
          " ",
          "Urban Riverfly",
          "Invasive Species"
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
          condition = "input.riverfly == 'ARMI'",
          includeMarkdown(app_sys("app/www/text/ARMI_description.md")),
          ns = ns
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.riverfly == 'Urban Riverfly species'",
        radioButtons(
          ns("riverflySpecies"),
          "Urban Riverfly species",
          choices = unname(unlist(riverfly_spp_bw))
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.riverfly == 'Other species'",
        radioButtons(
          ns("otherSpecies"),
          "Other species",
          choices = unname(unlist(other_spp_bw))
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.metric == 'Water Quality'",
        radioButtons(
          ns("readingType"),
          "Choose water quality reading type:",
          choices = c(
            "Conductivity (mS)" = "conductivity_mS",
            "Temperature (°C)" = "temperature_C",
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
      div(
        id = "yourdata-descriptor",
        HTML(
          "<b>Select the survey from the drop down menus and click on each point to view extra details</b>"
        )
      ), # Add this wrapper
      # Conditional panel for Urban Riverfly species
      conditionalPanel(
        condition = "input.metric == 'Urban Riverfly' && input.riverfly == 'Urban Riverfly species'",
        img(
          src = "www/images/Species_legend.png",
          id = "species-legend",
          alt = "Legend for Urban Riverfly species. Max abundance in the last three years. Options are >1000, 100-999, 10-99, and 1-9."
        ),
        ns = ns
      ),

      # Conditional panel for Other species
      conditionalPanel(
        condition = "input.metric == 'Urban Riverfly' && input.riverfly == 'Other species'",
        img(
          src = "www/images/Species_legend.png",
          id = "species-legend",
          alt = "Legend for Other species. Max abundance in the last three years. Options are >1000, 100-999, 10-99, and 1-9."
        ),
        ns = ns
      ),

      # Conditional panel for ARMI
      conditionalPanel(
        condition = "input.metric == 'Urban Riverfly' && input.riverfly == 'ARMI'",
        img(
          src = "www/images/ARMI_legend.png",
          id = "armi-legend",
          alt = "Legend for Anglers Riverfly Monitoring Initiative (ARMI) scores. Options are 0-3 (red), 4-5 (orange), 6-7 (yellow), 8-9 (light green), and 10+ (dark green)."
        ),
        ns = ns
      ),

      conditionalPanel(
        condition = "input.metric == 'Invasive Species' && (input.invasiveType == 'signal_crayfish' || input.invasiveType == 'killer_demon_shrimp')",
        img(
          src = "www/images/Invasive_fauna_legend.png",
          id = "invasive-fauna-legend",
          alt = "Legend for Invasive fauna species. Options are Present (red) and Not detected (green)."
        ),
        ns = ns
      ),

      # Conditional panel for invasive species - flora legend
      conditionalPanel(
        condition = "input.metric == 'Invasive Species' && (input.invasiveType == 'himalayan_balsam' || input.invasiveType == 'giant_hogweed' || input.invasiveType == 'japanese_knotweed')",
        img(
          src = "www/images/Invasive_flora_legend.png",
          id = "invasive-flora-legend",
          alt = "Legend for Invasive flora species. Options are Present (red) and Not detected (green)."
        ),
        ns = ns
      ),
      # Map: Use a separate class for the Leaflet map
      div(
        class = "leaflet-map-container",
        leaflet::leafletOutput(ns("map"))
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
#' @importFrom leaflet leafletProxy addProviderTiles setView clearMarkers addCircleMarkers addLegend clearControls
#' @importFrom leaflet providers leafletOptions renderLeaflet leaflet
#' @importFrom dplyr filter mutate rowwise
#' @noRd
mod_03_plot_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map <- renderLeaflet({
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
        setView(lng = -1.83, lat = 52.45, zoom = 10)
    })

    # Update the map with appropriate data
    updateMap <- function(input, output, session) {
      mapProxy <- leafletProxy("map")
      zoomLevel <- input$map_zoom
      clearMapLayers(mapProxy)
      addPolygonsAndLines(mapProxy, zoomLevel)
      mapProxy |> clearControls()

      # need to rename sampling_site across datasets
      Unique_BRC_Sampling_Locs <- read.csv(app_sys(
        "extdata/Unique_BRC_Sampling_Locs.csv"
      ))

      # Get the right data for ARMI
      con <- DBI::dbConnect(
        RSQLite::SQLite(),
        "data.sqlite",
        extended_types = TRUE
      )
      riverfly_data <- DBI::dbReadTable(con, "riverfly")
      dbDisconnect(con)

      Riverfly_Species_Plot_All <- species_plots(
        riverfly_data,
        Unique_BRC_Sampling_Locs
      )
      Riverfly_Species_Plot <- Riverfly_Species_Plot_All[[1]]
      Riverfly_Species_Plot_Recent <- Riverfly_Species_Plot_All[[2]]
      Riverfly_Other_Species_Plot <- Riverfly_Species_Plot_All[[3]]
      Riverfly_Other_Species_Plot_Recent <- Riverfly_Species_Plot_All[[4]]

      if (input$metric == "Urban Riverfly" && input$riverfly == "ARMI") {
        ARMI_assignment <- make_riverfly_ARMI(riverfly_data)
        ARMI_data <- sum_up_ARMI(ARMI_assignment)
        riverflyARMIDataList <- make_ARMI_plot_data(
          ARMI_data,
          Unique_BRC_Sampling_Locs
        )
        riverflyARMIData <- riverflyARMIDataList[[2]]
        Riverfly_ARMI_Plot <- riverflyARMIDataList[[1]]
        addARMIMarkers(mapProxy, riverflyARMIData, Riverfly_ARMI_Plot, input)
      } else if (
        input$metric == "Urban Riverfly" &&
          input$riverfly == "Urban Riverfly species"
      ) {
        # Filter by the selected Taxa
        selectedTaxa <- names(which(riverfly_spp_bw == input$riverflySpecies))

        riverflyspeciesData_Recent_Map <- Riverfly_Species_Plot_Recent |>
          filter(taxa == selectedTaxa)
        addRiverflySpeciesMarkers(
          mapProxy,
          riverflyspeciesData_Recent_Map,
          Riverfly_Species_Plot,
          selectedTaxa,
          input
        )
      } else if (
        input$metric == "Urban Riverfly" &&
          input$riverfly == "Other species"
      ) {
        # Filter data for the selected 'other species' from the radio buttons
        selectedTaxa <- names(which(other_spp_bw == input$otherSpecies))
        otherspeciesData_Recent_Map <- Riverfly_Other_Species_Plot_Recent |>
          filter(taxa == selectedTaxa)
        addOtherSpeciesMarkers(
          mapProxy,
          otherspeciesData_Recent_Map,
          selectedTaxa
        )
      } else if (input$metric == "Invasive Species") {
        plot_palette <- brewer.pal(n = 9, name = "RdBu")
        BRCInvSpcs_Plot_Recent <- make_recent_inv_spp(
          BRCInvSpcs,
          BRC_locs,
          plot_palette
        )

        addInvasiveSpeciesMarkers(
          mapProxy,
          BRCInvSpcs_Plot_Recent,
          input$invasiveType,
          plot_palette
        )
      }
    }

    observeEvent(
      {
        input$metric
        input$tabs
        input$invasiveType
        input$readingType
        input$score
        input$riverfly
        input$riverflySpecies
        input$otherSpecies
        input$map_zoom
      },
      {
        updateMap(input, output, session)
      }
    )
  })
}

## To be copied in the UI
# mod_03_plot_data_ui("03_plot_data_1")

## To be copied in the server
# mod_03_plot_data_server("03_plot_data_1")
