## Module: Data entry form for each DB table
## Provides UI and server to render inputs for every column (except id)

#' Data entry form UI
#'
#' @param id module id
#' @noRd
mod_data_entry_form_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("form_ui"))
    )
}

#' Data entry form server
#'
#' @param id module id
#' @param table_name character or reactive that names which table's inputs to render
#' @return a list with two reactives: values (named list) and submit (action button count)
#' @noRd
mod_data_entry_form_server <- function(id, table_name) {
    stopifnot(shiny::is.reactive(table_name) || is.character(table_name))

    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        # Define columns and types for each table (matching create_db.R)
        cols <- list(
            riverfly = c(
                organisation = "TEXT",
                survey_date = "TEXT",
                data_type = "TEXT",
                sampling_site = "TEXT",
                cased_caddisfly = "TEXT",
                caseless_caddisfly = "TEXT",
                olive_mayfly = "TEXT",
                blue_winged_olive_mayfly = "TEXT",
                freshwater_shrimp = "TEXT",
                freshwater_hoglouse = "TEXT",
                blackfly_larvae = "TEXT",
                freshwater_worm = "TEXT",
                freshwater_leech = "TEXT",
                freshwater_snail = "TEXT",
                freshwater_beetle = "TEXT",
                green_drake_mayfly = "TEXT",
                flat_bodied_stone_clinger_mayfly = "TEXT",
                stonefly_plecoptera = "TEXT",
                other_chironomidae = "TEXT",
                other_dicranota = "TEXT",
                other_tipulidae = "TEXT",
                other_hydracarina = "TEXT",
                other_hydropsychidae = "TEXT",
                other_rhyacophilidae = "TEXT",
                other_planorbidae = "TEXT",
                other_sphaeriidae = "TEXT",
                other_acroloxidae_ancylidae = "TEXT",
                other_bullhead = "TEXT"
            ),
            water_quality = c(
                organisation = "TEXT",
                survey_date = "TEXT",
                data_type = "TEXT",
                sampling_site = "TEXT",
                conductivity_mS = "REAL",
                temperature_C = "REAL",
                ammonia_ppm = "REAL",
                phosphate_ppm = "REAL",
                nitrate_ppm = "REAL",
                turbidity_NTU = "REAL",
                other_water_quality = "TEXT"
            ),
            riverfly_locs = c(
                sampling_site = "TEXT",
                Organisation = "TEXT",
                Easting = "INTEGER",
                Northing = "INTEGER",
                LAT = "REAL",
                LONG = "REAL"
            ),
            invasive_species = c(
                organisation = "TEXT",
                survey_date = "TEXT",
                data_type = "TEXT",
                invasive_spp_sampling_date = "TEXT",
                sampling_site = "TEXT",
                invasive_spp_what_three_words = "TEXT",
                signal_crayfish = "TEXT",
                killer_demon_shrimp = "TEXT",
                himalayan_balsam = "TEXT",
                japanese_knotweed = "TEXT",
                giant_hogweed = "TEXT",
                any_other_invasive_spp = "TEXT"
            ),
            outfall_safari = c(
                organisation = "TEXT",
                survey_date = "TEXT",
                data_type = "TEXT",
                outfall_survey_date = "TEXT",
                sampling_site = "TEXT",
                outfall_photo = "TEXT",
                outfall_flow = "TEXT",
                outfall_pollution_distance = "TEXT",
                outfall_aesthetics = "TEXT"
            ),
            riverflytest = c(
                organisation = "TEXT",
                survey_date = "TEXT",
                data_type = "TEXT",
                sampling_site = "TEXT",
                cased_caddisfly = "TEXT",
                caseless_caddisfly = "TEXT",
                olive_mayfly = "TEXT",
                blue_winged_olive_mayfly = "TEXT",
                freshwater_shrimp = "TEXT",
                freshwater_hoglouse = "TEXT",
                blackfly_larvae = "TEXT",
                freshwater_worm = "TEXT",
                freshwater_leech = "TEXT",
                freshwater_snail = "TEXT",
                freshwater_beetle = "TEXT",
                green_drake_mayfly = "TEXT",
                flat_bodied_stone_clinger_mayfly = "TEXT",
                stonefly_plecoptera = "TEXT",
                other_chironomidae = "TEXT",
                other_dicranota = "TEXT",
                other_tipulidae = "TEXT",
                other_hydracarina = "TEXT",
                other_hydropsychidae = "TEXT",
                other_rhyacophilidae = "TEXT",
                other_planorbidae = "TEXT",
                other_sphaeriidae = "TEXT",
                other_acroloxidae_ancylidae = "TEXT",
                other_bullhead = "TEXT"
            ),
            outfall_locs = c(
                Organisation = "TEXT",
                sampling_site = "TEXT",
                Easting = "INTEGER",
                Northing = "INTEGER",
                LAT = "REAL",
                LONG = "REAL"
            )
        )

        # helper to coerce table_name param to string
        current_table <- reactive({
            if (shiny::is.reactive(table_name)) {
                table_name()
            } else {
                as.character(table_name)
            }
        })

        # render UI for selected table
        output$form_ui <- shiny::renderUI({
            tbl <- current_table()
            if (is.null(tbl) || !tbl %in% names(cols)) {
                return(shiny::tagList(shiny::p(
                    "Select a valid data type to show the form."
                )))
            }

            items <- cols[[tbl]]
            ui_elems <- lapply(names(items), function(nm) {
                type <- items[[nm]]
                input_id <- nm
                label <- gsub("_", " ", nm)

                # choose widget by type or by name hints
                if (
                    nm %in%
                        c(
                            "survey_date",
                            "outfall_survey_date",
                            "invasive_spp_sampling_date"
                        )
                ) {
                    shiny::dateInput(ns(input_id), label = label, value = NULL)
                } else if (type == "INTEGER") {
                    shiny::numericInput(
                        ns(input_id),
                        label = label,
                        value = NA_integer_,
                        step = 1
                    )
                } else if (type == "REAL") {
                    shiny::numericInput(
                        ns(input_id),
                        label = label,
                        value = NA_real_,
                        step = 0.01
                    )
                } else if (grepl("photo|what_three_words|comment|other", nm)) {
                    shiny::textAreaInput(
                        ns(input_id),
                        label = label,
                        value = ""
                    )
                } else {
                    shiny::textInput(ns(input_id), label = label, value = "")
                }
            })

            # assemble with submit button
            shiny::tagList(
                shiny::wellPanel(
                    ui_elems,
                    shiny::actionButton(
                        ns("submit"),
                        "Submit",
                        class = "btn-primary"
                    )
                )
            )
        })

        # reactive that returns named list of inputs when requested
        values <- shiny::reactive({
            tbl <- current_table()
            if (is.null(tbl) || !tbl %in% names(cols)) {
                return(NULL)
            }
            nm <- names(cols[[tbl]])
            out <- setNames(vector("list", length(nm)), nm)
            for (n in nm) {
                out[[n]] <- input[[n]]
            }
            out
        })

        list(values = values, submit = shiny::reactive(input$submit))
    })
}
