#' 03_plot_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
          #"Invasive Species",
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
      div(
        id = "yourdata-descriptor",
        HTML(
          "<b>Select the survey from the drop down menus and click on each point to view extra details.</b> Points on the map may take a few seconds to load."
        )
      ),
      # Map: Use a separate class for the Leaflet map
      div(
        class = "leaflet-map-container",
        leaflet::leafletOutput(ns("map"), height = "100%")
      ),
      div(
        id = "attribution-statement",
        p(
          "Contains public sector information licensed under the",
          tags$a(
            href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
            "Open Government Licence v3.0"
          ),
          style = "text-align:right;font-size: 90%;"
        )
      )
    )
  )
}

#' 03_plot_data Server Functions
#' @importFrom leaflet leafletProxy addProviderTiles setView clearMarkers addCircleMarkers addLegend clearControls showGroup hideGroup
#' @importFrom leaflet providers leafletOptions renderLeaflet leaflet addLayersControl layersControlOptions
#' @importFrom dplyr filter mutate rowwise
#' @noRd
mod_03_plot_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Load the data and assign to variables in the module environment
    data_plot_list <- load_data()
    list2env(data_plot_list, envir = environment())

    # Initialize the Leaflet map
    output$map <- renderLeaflet({
      leaflet(
        options = leafletOptions(
          zoomControl = FALSE,
          attributionControl = FALSE,
          zoomSnap = 0.5
        )
      ) |>
        htmlwidgets::onRender(
          "function(el, x) {
          L.control.zoom({ position: 'bottomright' }).addTo(this)
      }"
        ) |>
        addProviderTiles(providers$OpenStreetMap) |>
        setView(lng = -1.82, lat = 52.455, zoom = 10.5) |>
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
      mapProxy <- leafletProxy("map")

      mapProxy |>
        leaflet::clearPopups() |>
        clearMapLayers()

      if (selected_metric() == "Urban Riverfly") {
        if (selected_riverfly() == "ARMI") {
          mapProxy <- leafletProxy("map") |>
            addARMIMarkers(
              map_data = riverflyARMIMap,
              popup_data = Riverfly_ARMI_Popups,
              screen_width = screen_width()
            ) |>
            showGroup("ARMI points")
        } else if (selected_riverfly() == "Urban Riverfly species") {
          # If the user chooses Urban Riverfly species, plot abundance data
          # Filter by the selected Taxa
          selectedTaxa <- names(which(
            riverfly_spp_bw == selected_riverfly_species()
          ))
          riverfly_species_popups <- Riverfly_Species_Plot[grepl(
            selectedTaxa,
            names(Riverfly_Species_Plot)
          )]
          mapProxy |>
            addRiverflySpeciesMarkers(
              popup_data = riverfly_species_popups,
              map_data = Riverfly_Species_Plot_Recent[[selectedTaxa]],
              selectedTaxa,
              screen_width()
            ) |>
            showGroup("Riverfly points")
        } else if (selected_riverfly() == "Other species") {
          # If the user chooses Other species, plot abundance data
          # Filter data for the selected 'other species' from the radio buttons
          selectedTaxa <- names(which(
            other_spp_bw == selected_other_species()
          ))
          otherspeciesData_Recent_Map <- Riverfly_Other_Species_Plot_Recent[[
            selectedTaxa
          ]]
          mapProxy |>
            addOtherSpeciesMarkers(
              otherspeciesData_Recent_Map,
              selectedTaxa
            ) |>
            showGroup("Other spp points")
        }
      } else if (selected_metric() == "Invasive Species") {
        # If the user chooses Invasive Species, plot presence/absence data
        mapProxy |>
          addInvasiveSpeciesMarkers(
            BRCInvSpcs_Plot_Recent,
            selected_invasive_type(),
            rev(brewer.pal(n = 4, name = "Blues"))
          ) |>
          showGroup("Invasive points")
      } else if (selected_metric() == "Water Chemistry") {
        # If the user chooses Water Chemistry, plot water quality data

        wq_Recent_Map <- WQ_plot_data$recent[[selected_reading_type()]]

        wq_data <- WQ_plot_data$all_obs[grepl(
          selected_reading_type(),
          names(WQ_plot_data$all_obs)
        )]

        mapProxy |>
          addWaterQualityMarkers(
            wq_data = wq_data,
            wq_data_recent = wq_Recent_Map,
            metric = selected_reading_type(),
            screen_width = screen_width()
          )

        mapProxy |>
          showGroup("Water Quality points")
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
          input$otherSpecies
        )
      },
      {
        updateMap(input, output, session)
      },
      ignoreInit = TRUE
    )

    # Adapt the performance branch's click-to-plot flow to main's preloaded
    # site-split datasets: build only the selected site's plot after a click,
    # rather than rendering a plot for every marker whenever the map updates.
    observeEvent(
      input$map_marker_click,
      {
        click <- input$map_marker_click
        req(click$id)

        # Marker IDs are sequential row indexes; guard against stale or invalid
        # clicks before using the ID to subset the currently selected dataset.
        marker_index <- suppressWarnings(as.integer(click$id))
        req(
          length(marker_index) == 1L,
          !is.na(marker_index),
          marker_index >= 1L
        )

        if (
          selected_metric() == "Urban Riverfly" &&
            selected_riverfly() %in% c("ARMI", "Urban Riverfly species")
        ) {
          # Preserve the popup sizing used by the existing map plots.
          dimensions <- if (!is.null(screen_width()) && screen_width() <= 480) {
            c(width = 300, height = 250)
          } else if (!is.null(screen_width()) && screen_width() <= 768) {
            c(width = 400, height = 275)
          } else {
            c(width = 600, height = 350)
          }
          title_wrap_width <- if (dimensions[["width"]] <= 300) {
            37
          } else if (dimensions[["width"]] <= 450) {
            50
          } else {
            75
          }
          browser()
          if (selected_riverfly() == "ARMI") {
            # ARMI marker rows hold site IDs; the full dated observations are
            # already split by site in Riverfly_ARMI_Popups.
            req(marker_index <= nrow(riverflyARMIMap))
            selected_marker <- riverflyARMIMap[marker_index, , drop = FALSE]
            site_id <- selected_marker$sampling_site[[1]]
            plot_data <- Riverfly_ARMI_Popups[[site_id]]
            req(!is.null(plot_data), nrow(plot_data) > 0)

            date_range <- range(plot_data$survey_date, na.rm = TRUE)
            req(all(is.finite(as.numeric(date_range))))
            # A single observation still needs a non-zero x-axis interval.
            if (diff(date_range) == 0) {
              date_range <- date_range + c(-15, 15)
            }

            organisation <- selected_marker$organisation[[1]]
            if (
              !organisation %in%
                c(
                  "Hall Green's Keepin' It Clean",
                  "Birmingham Conservation Society"
                )
            ) {
              organisation <- paste("the", organisation)
            }

            popup <- make_armi_popup(
              row_data = plot_data,
              breaks_vector = filter(plot_breaks, reading_type == "ARMI") |>
                select(bin_breaks) |>
                unlist(),
              date_range = date_range,
              title_text = paste0(
                "ARMI score at ",
                site_id,
                ". Sampled by ",
                organisation,
                "."
              ),
              title_wrap_width = title_wrap_width,
              width = dimensions[["width"]],
              height = dimensions[["height"]]
            )
          } else {
            # Species markers are drawn from the recent, NA-filtered subset, so
            # use that same row order to find the site's complete time series.
            selected_taxa <- names(which(
              riverfly_spp_bw == selected_riverfly_species()
            ))
            req(length(selected_taxa) == 1L)
            map_points <- Riverfly_Species_Plot_Recent[[selected_taxa]] |>
              tidyr::drop_na()
            req(marker_index <= nrow(map_points))

            site_id <- map_points$sampling_site[[marker_index]]
            plot_data <- Riverfly_Species_Plot[[
              paste0(selected_taxa, ".", site_id)
            ]]
            req(!is.null(plot_data), nrow(plot_data) > 0)
            # Missing abundance denotes a negative observation in these records.
            plot_data <- plot_data |>
              dplyr::mutate(abundance = tidyr::replace_na(abundance, 0))

            date_range <- range(plot_data$survey_date, na.rm = TRUE)
            req(all(is.finite(as.numeric(date_range))))
            if (diff(date_range) == 0) {
              date_range <- date_range + c(-15, 15)
            }

            taxa_common_name <- gsub(
              "\\s*\\([^\\)]+\\)",
              "",
              riverfly_spp_bw[[selected_taxa]]
            )
            if (taxa_common_name == "Flat-bodied stone clinger mayfly") {
              taxa_common_name <- "Stone clinger mayfly"
            }

            organisation <- plot_data$organisation[[1]]
            if (
              !organisation %in%
                c(
                  "Hall Green's Keepin' It Clean",
                  "Birmingham Conservation Society"
                )
            ) {
              organisation <- paste("the", organisation)
            }

            popup <- make_riverflySpecies_popup(
              row_data = plot_data,
              breaks_vector = c(-Inf, 0:4),
              date_range = date_range,
              title_text = paste0(
                taxa_common_name,
                " numbers at ",
                site_id,
                ". Sampled by ",
                organisation,
                "."
              ),
              title_wrap_width = title_wrap_width,
              width = dimensions[["width"]],
              height = dimensions[["height"]]
            )
          }

          # Replace any previous click popup so only the newly selected plot is
          # shown at the marker's coordinates.
          leafletProxy("map") |>
            leaflet::clearPopups() |>
            leaflet::addPopups(lng = click$lng, lat = click$lat, popup = popup)
        }
      },
      ignoreInit = TRUE
    )
  })
}

## To be copied in the UI
# mod_03_plot_data_ui("03_plot_data_1")

## To be copied in the server
# mod_03_plot_data_server("03_plot_data_1")
