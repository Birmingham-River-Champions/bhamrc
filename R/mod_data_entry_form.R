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
                other_bullhead = "TEXT",
                other_taxa_1 = "TEXT"
            ),
            water_quality = c(
                organisation = "TEXT",
                survey_date = "TEXT",
                data_type = "TEXT",
                sampling_site = "TEXT",
                conductivity_mS = "REAL",
                temperature_C = "REAL",
                ammonia_ppm = "REAL",
                phosphate_ppm = "TEXT",
                nitrate_ppm = "TEXT",
                turbidity_NTU = "TEXT",
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
                data_type = "TEXT",
                survey_date = "TEXT",
                sampling_site = "TEXT",
                signal_crayfish = "TEXT",
                killer_demon_shrimp = "TEXT",
                himalayan_balsam = "TEXT",
                japanese_knotweed = "TEXT",
                giant_hogweed = "TEXT",
                any_other_invasive_spp = "TEXT",
                invasive_spp_wtw = "TEXT"
            ),
            outfall_safari = c(
                organisation = "TEXT",
                data_type = "TEXT",
                survey_date = "TEXT",
                sampling_site = "TEXT",
                outfall_photo = "TEXT",
                outfall_flow = "TEXT",
                outfall_pollution_distance = "TEXT",
                outfall_aesthetics = "TEXT",
                other_pollution_description = "TEXT",
                outfall_location_wtw = "TEXT"
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

        con <-
            DBI::dbConnect(
                RSQLite::SQLite(),
                "data.sqlite",
                extended_types = TRUE
            )
        locations_tbl_riverfly <- dbReadTable(
            con,
            "riverfly_locs"
        )

        locations_tbl_outfall <- dbReadTable(
            con,
            "outfall_locs"
        )

        DBI::dbDisconnect(con)

        organisation_choices <-
            sort(unique(locations_tbl_riverfly$Organisation))

        site_choices_riverfly <-
            sort(unique(locations_tbl_riverfly$sampling_site))

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
            tbl_name <- data_types_bw[[which(names(data_types_bw) == tbl)]]

            if (is.null(tbl) || !tbl_name %in% names(cols)) {
                return(shiny::tagList(shiny::p(
                    "Select a valid data type to show the form."
                )))
            }

            items <- cols[[tbl_name]]

            ui_elems <- lapply(names(items), function(column_name) {
                type <- items[[column_name]]
                input_id <- column_name
                label <- ifelse(
                    column_name %in% names(survey_questions),
                    survey_questions[[column_name]],
                    gsub("_", " ", column_name)
                )
                # Choose type of input control by type or by name hints
                if (column_name == "survey_date") {
                    shiny::dateInput(
                        ns(input_id),
                        label = with_red_star(label),
                        value = NULL,
                        max = Sys.Date()
                    )
                } else if (column_name == "organisation") {
                    shiny::selectInput(
                        ns(input_id),
                        label = with_red_star(label),
                        choices = c(
                            "Select organisation" = "",
                            organisation_choices
                        ),
                        selected = NULL
                    )
                } else if (column_name == "sampling_site") {
                    shiny::selectInput(
                        ns(input_id),
                        label = with_red_star(label),
                        choices = c(
                            "Select sampling site" = "",
                            site_choices_riverfly
                        )
                    )
                } else if (column_name == "data_type") {} else if (
                    type == "INTEGER"
                ) {
                    shiny::numericInput(
                        ns(input_id),
                        label = label,
                        value = NA_integer_,
                        step = 1
                    )
                } else if (grepl("wtw", column_name)) {
                    shiny::tagList(
                        img(
                            src = "www/images/whatthreewords.png",
                            alt = "Image of what3words.com webpage showing a geolocate button",
                            height = 300,
                            width = 150
                        ),
                        shiny::textAreaInput(
                            ns(input_id),
                            label = label,
                            value = ""
                        )
                    )
                } else if (
                    # If this is a column with the specified abundance bins, use the selectInput
                    column_name %in%
                        c(
                            names(riverfly_spp_bw),
                            c("killer_demon_shrimp", "signal_crayfish")
                        )
                ) {
                    shiny::tagList(
                        img(
                            src = paste0(
                                "www/images/",
                                column_name,
                                ".png"
                            ),
                            width = 100,
                            height = 100,
                            alt = column_name
                        ),
                        shiny::selectInput(
                            ns(input_id),
                            label = label,
                            choices = choices_list$abundance,
                            selected = "0"
                        )
                    )
                } else if (column_name %in% names(other_spp_bw)) {
                    shiny::radioButtons(
                        ns(input_id),
                        label = label,
                        choices = choices_list$abundance,
                        selected = "0",
                        inline = TRUE
                    )
                } else if (column_name == "other_taxa_1") {
                    shiny::tagList(
                        extra_taxa_input_ui(
                            ns("extra_taxa_1"),
                            label = survey_questions$other_taxa_1
                        ),
                        shiny::actionButton(
                            ns("add_taxa"),
                            label = "Add another taxa observation",
                            class = "btn-primary"
                        )
                    )
                } else if (
                    # If invasive flora, use present/abundant choices
                    column_name %in%
                        c(
                            "himalayan_balsam",
                            "japanese_knotweed",
                            "giant_hogweed"
                        )
                ) {
                    shiny::tagList(
                        img(
                            src = paste0(
                                "www/images/",
                                column_name,
                                ".jpg"
                            ),
                            width = 166,
                            height = 250,
                            alt = column_name
                        ),
                        shiny::radioButtons(
                            ns(input_id),
                            label = label,
                            choices = choices_list$invasive_flora,
                            selected = NULL
                        )
                    )
                } else if (
                    grepl(
                        "phosphate_ppm|nitrate_ppm|turbidity_NTU",
                        column_name
                    )
                ) {
                    shiny::tagList(
                        img(
                            src = paste0(
                                "www/images/",
                                column_name,
                                ".png"
                            ),
                            width = 168,
                            height = 120,
                            alt = column_name
                        ),
                        shiny::selectInput(
                            ns(input_id),
                            label = label,
                            choices = choices_list[[label]],
                            selected = "Not measured"
                        )
                    )
                } else if (
                    grepl(
                        "conductivity_mS|temperature_C|ammonia_ppm",
                        column_name
                    )
                ) {
                    shiny::tagList(
                        img(
                            src = paste0(
                                "www/images/",
                                column_name,
                                ".png"
                            ),
                            width = 100,
                            height = 100,
                            alt = column_name
                        ),
                        shiny::numericInput(
                            ns(input_id),
                            label = label,
                            value = NA_real_,
                            step = 0.01
                        )
                    )
                } else if (column_name == "outfall_photo") {
                    shiny::tagList(
                        HTML(label),
                        shiny::checkboxInput(
                            ns(input_id),
                            label = "Yes",
                            value = FALSE
                        ),
                        shiny::checkboxInput(
                            ns(input_id),
                            label = "No",
                            value = FALSE
                        )
                    )
                } else if (
                    column_name %in%
                        c(
                            "outfall_flow",
                            "outfall_pollution_distance",
                            "outfall_aesthetics"
                        )
                ) {
                    shiny::radioButtons(
                        ns(input_id),
                        label = with_red_star(label),
                        choices = choices_list[[column_name]],
                        selected = NULL
                    )
                } else {
                    shiny::textInput(ns(input_id), label = label, value = "")
                }
            })

            # assemble with submit button
            shiny::tagList(
                shiny::wellPanel(
                    shiny::tagList(
                        tags$h1(tbl),
                        shiny::tags$div(id = ns("outfall_images")),
                        shiny::textInput(
                            ns("email"),
                            label = with_red_star("Email"),
                            value = NULL
                        ),
                        ui_elems,
                        # placeholder container for inserted extra taxa UI (insertUI will target this)
                        shiny::tags$div(id = ns("extra_container")),
                        shiny::actionButton(
                            ns("submit"),
                            "Submit",
                            class = "btn-primary"
                        )
                    )
                )
            )
        })

        mandatory_fields <- c(
            "organisation",
            "sampling_site",
            "survey_date",
            "email"
        )

        # Add in the Urban Outfall Safari image if the outfall data type is selected
        observe({
            if (current_table() == "Urban Outfall Safari") {
                container_sel <- paste0("#", session$ns("outfall_images"))
                shiny::insertUI(
                    selector = container_sel,
                    where = "beforeEnd",
                    ui = shiny::tags$div(
                        id = "outfall_image",
                        tags$p("Outfall safari pollution examples"),
                        tags$img(
                            src = "www/images/outfall.png",
                            width = 417,
                            height = 382,
                            alt = "Outfall flow options"
                        )
                    )
                )
                mandatory_fields <- c(
                    mandatory_fields,
                    "outfall_flow",
                    "outfall_pollution_distance",
                    "outfall_aesthetics"
                )
            }
        })

        lapply(
            X = c(mandatory_fields, "02_data_input_1-data_type-data_type"),
            FUN = function(i) {
                observeEvent(input[[i]], {
                    shinyjs::toggleState(
                        id = "submit",
                        condition = toggle_submit(mandatory_fields)
                    )
                })
            }
        )

        toggle_submit <- function(mandatory_fields) {
            mandatory_filled <-
                vapply(
                    mandatory_fields,
                    function(x) {
                        !is.null(input[[x]]) && input[[x]] != ""
                    },
                    logical(1)
                )

            mandatory_filled <- all(mandatory_filled)
            return(mandatory_filled)
        }

        # Append additional extra taxa UI entries on each click of the namespaced "extra" button.
        # Each appended block gets a unique wrapper id and its own remove button so users can
        # remove any appended block individually.
        extra_counter <- shiny::reactiveVal(0)
        observeEvent(input$add_taxa, {
            # increment counter
            cnt <- extra_counter() + 1
            extra_counter(cnt)

            if (cnt < 8) {
                # build ids (local id and namespaced wrapper id)
                wrapper_local_id <- paste0("extra_wrapper_", cnt)
                remove_btn_local_id <- paste0("remove_extra_", cnt)
                wrapper_ns_id <- session$ns(wrapper_local_id)
                container_sel <- paste0("#", session$ns("extra_container"))

                # insert the new block at the end of the placeholder container
                shiny::insertUI(
                    selector = container_sel,
                    where = "beforeEnd",
                    ui = shiny::tags$div(
                        id = wrapper_ns_id,
                        extra_taxa_input_ui(
                            ns(paste0("extra_taxa_", cnt)),
                            label = survey_questions$other_taxa_1
                        ),
                        shiny::actionButton(
                            ns(remove_btn_local_id),
                            "Remove",
                            class = "btn-danger btn-sm"
                        )
                    )
                )
            } else {
                shiny::showNotification(
                    "You cannot add any more taxa; please start a new entry if you have additional taxa to report.",
                    type = "message"
                )
            }
        })

        # Single observer for all remove buttons
        observe({
            # Find all remove button ids currently present in input
            remove_btn_ids <- grep("^remove_extra_", names(input), value = TRUE)
            lapply(remove_btn_ids, function(x) {
                observeEvent(
                    input[[x]],
                    {
                        # Remove the corresponding UI block
                        wid <- session$ns(sub("^remove_", "", x))
                        shiny::removeUI(selector = paste0("#", wid))
                    },
                    ignoreInit = TRUE,
                    once = TRUE
                )
            })
        })

        # Make sure entries are valid before submitting
        # Check that the email address is valid, temperature, conductivity, and ammonia are within expected ranges.
        observeEvent(input$submit, {
            if (!isValidEmail(input$email)) {
                shiny::showNotification(
                    "Please enter a valid email address.",
                    type = "warning"
                )
            }
            if (
                (!is.null(input$conductivity_mS) &&
                    !is.na(input$conductivity_mS) &&
                    (input$conductivity_mS < 200 ||
                        input$conductivity_mS > 800)) ||
                    (!is.null(input$temperature_C) &&
                        !is.na(input$temperature_C) &&
                        (input$temperature_C < 0 ||
                            input$temperature_C > 35)) ||
                    (!is.null(input$ammonia_ppm) &&
                        !is.na(input$ammonia_ppm) &&
                        (input$ammonia_ppm < 0 || input$ammonia_ppm > 2))
            ) {
                shiny::showNotification(
                    "You have entered a value which we suspect may be an anomalous value. 
                    If you are confident this is correct, please proceed with the submission 
                    by leaving blank for now and email birminghamriverchampions@gmail.com.  
                    If  your conductivity meter says mS you can simply multiply your value by 1000",
                    type = "warning"
                )
            }
            if (input$organisation == "") {
                shiny::showNotification(
                    "You need to provide an organisation to submit this entry.",
                    type = "warning"
                )
            }
            if (input$sampling_site == "") {
                shiny::showNotification(
                    "You need to provide a sampling site to submit this entry.",
                    type = "warning"
                )
            }
            if (input$survey_date == Sys.Date()) {
                shiny::showNotification(
                    "You've submitted today's date as the sampling date. Is that correct?",
                    type = "warning"
                )
            }
        })

        # reactive that returns named list of inputs when requested
        values <- shiny::reactive({
            tbl <- current_table()
            if (is.null(tbl) || !tbl %in% names(cols)) {
                return(NULL)
            }
            column_name <- names(cols[[tbl]])
            out <- setNames(
                vector("list", length(column_name)),
                column_name
            )
            for (n in column_name) {
                out[[n]] <- input[[n]]
            }
            out
        })

        list(
            values = values,
            submit = shiny::reactive(input$submit)
        )
    })
}
