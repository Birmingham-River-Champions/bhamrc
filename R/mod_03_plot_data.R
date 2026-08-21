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
        leaflet::leafletOutput(
          ns("submission_map"),
          height = "calc(100vh - 150px)"
        )
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
              popup_data = riverfly_species_popups,
              map_data = be_result()$Riverfly_Species_Plot_Recent[[
                selectedTaxa
              ]],
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
          otherspeciesData_Recent_Map <- be_result()$Riverfly_Other_Species_Plot_Recent[[
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
        # Plot invasiv species
        # If the user chooses Invasive Species, plot presence/absence data
        mapProxy |>
          addInvasiveSpeciesMarkers(
            be_result()$BRCInvSpcs_Plot_Recent,
            selected_invasive_type(),
            rev(brewer.pal(n = 4, name = "Blues"))
          ) |>
          showGroup("Invasive points")
      } else if (selected_metric() == "Water Chemistry") {
        # If the user chooses Water Chemistry, plot water quality data

        wq_Recent_Map <- be_result()$WQ_plot_data$recent[[selected_reading_type()]]

        wq_data <- be_result()$WQ_plot_data$all_obs[grepl(
          selected_reading_type(),
          names(be_result()$WQ_plot_data$all_obs)
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
          input$otherSpecies,
          be_result()$df_geolocated_submissions
        )
      },
      {
        req(be_result()$df_geolocated_submissions)
        mapProxy <- leafletProxy("submission_map")
        mapProxy |>
          clearMarkers()
        updateMap(input, output, session)
      },
      ignoreInit = TRUE
    )

    output$click_debug <- renderText({
      click <- input$submission_map_marker_click
      if (is.null(click)) {
        "No click yet"
      } else {
        paste(
          "Clicked marker id:",
          click$id,
          "| lat:",
          click$lat,
          "| lng:",
          click$lng
        )
      }
    })

    observeEvent(input$submission_map_marker_click, {
      click <- input$submission_map_marker_click
      req(click$id)

      mapProxy <- leafletProxy("submission_map")

      # look up the row from whichever dataset is currently selected
      # (mirrors the branching logic already in updateMap())
      if (selected_metric() == "Urban Riverfly") {
        if (selected_riverfly() == "ARMI") {
          row <- be_result()$riverflyARMIMap[click$id, ]
          plot_data <- be_result()$Riverfly_ARMI_Popups[[unique(
            row$sampling_site
          )]]

          breaks_vector <- filter(plot_breaks, reading_type == "ARMI") |>
            select(bin_breaks) |>
            unlist()

          pal_name <- "RdBu"
          pal <- colorBin(
            palette = pal_name,
            #domain = map_data$ARMI,
            bins = breaks_vector,
            pretty = FALSE
          )

          # Calculate date range buffer if there's only one sample
          date_range <- range(
            plot_data$survey_date,
            na.rm = TRUE
          )

          if (diff(date_range) == 0) {
            date_range <- c(date_range[1] - 15, date_range[2] + 15)
          }

          # Screen width appears unavailable here! TODO: Investigate

          screen_width <- screen_width()
          # Adjust plot size based on screen width
          if (!is.null(screen_width)) {
            if (screen_width <= 480) {
              # For small screens like iPhones
              popup_width <- 300
              popup_height <- 250
            } else if (screen_width <= 768) {
              # For tablets
              popup_width <- 400
              popup_height <- 275
            } else {
              # For larger screens
              popup_width <- 600
              popup_height <- 350
            }
          } else {
            popup_width <- 600
            popup_height <- 350
          }

          site_id <- row$sampling_site[1]
          organisation <- row$organisation[1]

          # Set character width for str_wrap based on popup width
          title_wrap_width <- ifelse(
            popup_width <= 300,
            37,
            ifelse(popup_width <= 450, 50, 75)
          )
          title_text <- paste0(
            "ARMI score at ",
            site_id,
            ". Sampled by ",
            organisation,
            "."
          )

          p <- make_armi_popup(
            #row,
            plot_data,
            breaks_vector,
            date_range,
            title_text,
            title_wrap_width,
            popup_width,
            popup_height
          )

          mapProxy |>
            clearPopups() |>
            addPopups(
              lng = click$lng,
              lat = click$lat,
              popup = p
            )
        } else if (selected_riverfly() == "Urban Riverfly species") {
          # other branches per dataset
          selectedTaxa <- names(which(
            riverfly_spp_bw == selected_riverfly_species()
          ))
          riverfly_species_popups <- be_result()$Riverfly_Species_Plot[grepl(
            selectedTaxa,
            names(be_result()$Riverfly_Species_Plot)
          )]

          map_points <- be_result()$Riverfly_Species_Plot_Recent[[selectedTaxa]]

          riverflyspeciesData_Recent_Map <- map_points |> drop_na()

          # If no records for the specific taxaType, display popup message
          if (nrow(riverflyspeciesData_Recent_Map) == 0) {
            # Handle no data case
            if (all(is.na(data$LONG)) || all(is.na(data$LAT))) {
              default_lng <- -1.89983 # Example: center of Birmingham
              default_lat <- 52.48624 # Example: center of Birmingham
            } else {
              default_lng <- mean(data$LONG, na.rm = TRUE)
              default_lat <- mean(data$LAT, na.rm = TRUE)
            }

            mapProxy |>
              addPopups(
                lng = default_lng,
                lat = default_lat,
                popup = "<div style='text-align:center;'><strong>No project records currently</strong></div>",
                options = popupOptions(
                  closeButton = TRUE,
                  closeOnClick = FALSE
                )
              )
          } else {
            # popup_data <- riverfly_species_popups[[selectedTaxa]]
            # Get all popup data containing desired Taxa across locations
            site_id <- map_points[click$id, ]$sampling_site[1]

            plot_data <- riverfly_species_popups[
              names(riverfly_species_popups) ==
                paste0(
                  selectedTaxa,
                  ".",
                  site_id
                )
            ]

            # pull out of list
            plot_data <- plot_data[[1]]

            organisation <- plot_data$organisation[1]

            current_breaks <- c(-Inf, 0:4)
            pal <- colorFactor(
              palette = levels(
                riverflyspeciesData_Recent_Map$Riverfly_Species_Colour
              ),
              domain = riverflyspeciesData_Recent_Map$Riverfly_Species_Colour
            )

            # Get all Urban Riverfly species data for this specific site and taxa
            # Turn NAs into 0s since these should be negative abundance observations, not lack of sampling
            riverflyspeciesData_All_ggplot <- riverfly_species_popups[[paste0(
              selectedTaxa,
              ".",
              site_id
            )]] |>
              mutate(abundance = tidyr::replace_na(abundance, 0))

            # Custom changing of some organisations (those ) for "Flat bodied stone clinger mayfly"
            organisation <- if (
              organisation != "Hall Green's Keepin' It Clean" &
                organisation != "Birmingham Conservation Society"
            ) {
              organisation <- paste("the", organisation)
            } else {
              organisation <- organisation # This line is optional, just for clarity
            }
            # Custom shortening for "Flat bodied stone clinger mayfly"
            taxaType_CommonName <- gsub(
              "\\s*\\([^\\)]+\\)",
              "",
              riverfly_spp_bw[[selectedTaxa]]
            )
            if (taxaType_CommonName == "Flat-bodied stone clinger mayfly") {
              taxaType_CommonName <- "Stone clinger mayfly"
            }

            # Calculate date range buffer if there's only one sample
            date_range <- range(
              riverflyspeciesData_All_ggplot$survey_date,
              na.rm = TRUE
            )
            if (diff(date_range) == 0) {
              date_range <- c(date_range[1] - 15, date_range[2] + 15)
            }

            screen_width <- screen_width()
            # Adjust plot size based on screen width
            if (!is.null(screen_width)) {
              if (screen_width <= 480) {
                # For small screens like iPhones
                popup_width <- 300
                popup_height <- 250
              } else if (screen_width <= 768) {
                # For tablets
                popup_width <- 400
                popup_height <- 275
              } else {
                # For larger screens
                popup_width <- 600
                popup_height <- 350
              }
            } else {
              popup_width <- 600
              popup_height <- 350
            }

            # Set character width for str_wrap based on popup width
            title_wrap_width <- ifelse(
              popup_width <= 300,
              37,
              ifelse(popup_width <= 450, 50, 75)
            )
            title_text <- paste0(
              "ARMI score at ",
              site_id,
              ". Sampled by ",
              organisation,
              "."
            )

            p <- make_riverflySpecies_popup(
              riverflyspeciesData_All_ggplot,
              current_breaks,
              date_range,
              title_text,
              title_wrap_width,
              popup_width,
              popup_height
            )

            mapProxy |>
              clearPopups() |>
              addPopups(
                lng = click$lng,
                lat = click$lat,
                popup = p
              )
          }

          #row <- be_result()$
        } else if (selected_riverfly() == "Other Species") {
          # if the user chooses other species plot abundance data
          # Blank as no plots currently created for this datatset
        }
      } else if (selected_metric() == 'Invasive Species') {
        # Invasive species
        # Blank as no plots currently created for this datatset
      } else if (selected_metric() == 'Water Chemistry') {
        # Water Chemistry
        mapProxy |>
          clearPopups() |>
          addPopups(lng = click$lng, lat = click$lat, popup = p)
      }
    })

    # output$click_debug <- renderText({
    #   click <- input$submission_map_marker_click
    #   browser()
    #   if (is.null(click)) {
    #     "No click yet"
    #   } else {
    #     paste(
    #       "Clicked marker id:",
    #       click$id,
    #       "| lat:",
    #       click$lat,
    #       "| lng:",
    #       click$lng
    #     )
    #   }
    # })
    # # Observer to trigger when user clieks on a map marker
    #observeEvent(input$map_marker_click, {
    #click <- input$map_marker_click
    #message('you clicked, well done')
    #   req(click$id)
    #   message(click$id)
    #})

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
