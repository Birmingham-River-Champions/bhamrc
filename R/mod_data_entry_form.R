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
                other_unspecified_1 = "TEXT",
                other_unspecified_2 = "TEXT",
                other_unspecified_3 = "TEXT",
                other_unspecified_4 = "TEXT",
                other_unspecified_5 = "TEXT",
                other_unspecified_6 = "TEXT",
                other_unspecified_7 = "TEXT",
                other_unspecified_8 = "TEXT",
                names_of_other_taxa = "TEXT"
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

        # Get the organisation choices for riverfly for drop down list
        organisation_choices <-
            sort(unique(locations_tbl_riverfly$Organisation))

        # Get the sampling site choices for riverfly for drop down list
        site_choices_riverfly <-
            sort(unique(locations_tbl_riverfly$sampling_site))

        # Columns in the riverfly table that should not have inputs created
        cols_to_not_create <- c(
            "id",
            "data_type",
            paste0("other_unspecified_", 2:8),
            "names_of_other_taxa"
        )

        # List fields that should require the form to be greyed out if not completed.
        mandatory_fields <- c(
            "organisation",
            "sampling_site",
            "survey_date",
            "email"
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
            # Get the reactive value of the currently-selected table
            tbl <- current_table()

            # Get the programmatic table name from the human-readable name
            tbl_name <- data_types_bw[[which(names(data_types_bw) == tbl)]]

            # If an invalid table is selected, show a message
            if (is.null(tbl) || !tbl_name %in% names(cols)) {
                return(shiny::tagList(shiny::p(
                    "Select a valid data type to show the form."
                )))
            }

            # Create a list of form inputs that matches the specified columns for each table
            items <- cols[[tbl_name]]

            # For each item in the columns, create the appropriate input control
            # Get its type, ID, and label. Use the column name without underscores as a fallback column name.
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
                } else if (column_name %in% cols_to_not_create) {} else if (
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
                } else if (column_name == "other_unspecified_1") {
                    shiny::tagList(
                        extra_taxa_input_ui(
                            ns("other_unspecified_1"),
                            label = survey_questions$other_unspecified_1
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
                        # Placeholder div for pollution image, not shown for other tabs
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

        # Add in the Urban Outfall Safari image if the outfall data type is selected
        # Use observe rather than observeEvent to catch changes in table selection
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

        # Helper function to determine if all mandatory fields are filled
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
        extra_counter <- shiny::reactiveVal(1)
        observeEvent(input$add_taxa, {
            # increment counter
            cnt <- extra_counter() + 1
            extra_counter(cnt)

            if (cnt < 9) {
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
                            ns(paste0("other_unspecified_", cnt)),
                            label = survey_questions$other_unspecified_1
                        ),
                        shiny::actionButton(
                            ns(remove_btn_local_id),
                            "Remove",
                            class = "btn-danger btn-sm"
                        )
                    )
                )

                # create an observer to handle removal of this specific block when its Remove button is clicked
                local({
                    rid <- remove_btn_local_id
                    wid <- wrapper_ns_id
                    observeEvent(
                        input[[rid]],
                        {
                            shiny::removeUI(selector = paste0("#", wid))
                        },
                        ignoreInit = TRUE,
                        once = TRUE
                    )
                })
            } else {
                shiny::showNotification(
                    "You cannot add any more taxa; please start a new entry if you have additional taxa to report.",
                    type = "message"
                )
            }
        })

        # reactive that returns named list of inputs when requested
        values <- shiny::reactive({
            tbl <- current_table()
            if (is.null(tbl) || !tbl %in% names(cols)) {
                return(NULL)
            }

            tbl_name <- data_types_bw[[which(names(data_types_bw) == tbl)]]
            column_name <- names(cols[[tbl_name]])
            out <- setNames(
                vector("list", length(column_name)),
                column_name
            )
            for (n in column_name) {
                out[[n]] <- input[[n]]
            }
            out
        })

        allow_submit <- shiny::reactiveVal(TRUE)

        # Make sure entries are valid before submitting
        # Check that the email address is valid, temperature, conductivity, and ammonia are within expected ranges.
        observeEvent(input$submit, {
            if (!isValidEmail(input$email)) {
                shiny::showNotification(
                    "Please enter a valid email address.",
                    type = "warning"
                )
                allow_submit(FALSE)
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
                allow_submit(FALSE)
            }
            if (input$organisation == "") {
                shiny::showNotification(
                    "You need to provide an organisation to submit this entry.",
                    type = "warning"
                )
                allow_submit(FALSE)
            }
            if (input$sampling_site == "") {
                shiny::showNotification(
                    "You need to provide a sampling site to submit this entry.",
                    type = "warning"
                )
                allow_submit(FALSE)
            }
            if (input$survey_date == Sys.Date()) {
                shiny::showNotification(
                    "You've submitted today's date as the sampling date. Is that correct?",
                    type = "warning"
                )
            }

            # Check that the input data is valid and then submit if so
            values <- shiny::isolate(values())
            con <- DBI::dbConnect(
                RSQLite::SQLite(),
                "data.sqlite",
                extended_types = TRUE
            )
            tbl <- current_table()
            tbl_name <- data_types_bw[[which(names(data_types_bw) == tbl)]]

            existing_data <- DBI::dbReadTable(con, tbl_name)

            if (tbl_name == "outfall_safari") {
                locations_data <- DBI::dbReadTable(con, "outfall_locs")
            } else {
                locations_data <- DBI::dbReadTable(con, "riverfly_locs")
            }

            # Check if the sampling site exists for the selected organisation
            site_orgs <- paste(
                input$organisation,
                input$sampling_site
            )
            acceptable_site_orgs <- acceptable_locs(locations_data)
            if (!(site_orgs %in% acceptable_site_orgs$identifiers)) {
                shiny::showNotification(
                    "The selected sampling site does not match the selected organisation. Please correct this before submitting.",
                    type = "error"
                )
                allow_submit(FALSE)
            }

            #Create an empty row to populate
            new_row <- existing_data[0, ]
            other_taxa_names <- c()

            # Loop through the fields and add the data to new_row
            for (colname in names(cols[[tbl_name]])) {
                if (not_null(input[[colname]])) {
                    # Put entered value into new row at the appropriate column
                    if (colname %in% names(existing_data)) {
                        if (colname == "survey_date") {
                            # If input is a date, convert to the desired format
                            new_row[1, colname] <- paste(
                                lubridate::day(input[[colname]]),
                                lubridate::month(input[[colname]]),
                                lubridate::year(input[[colname]]),
                                sep = "/"
                            )
                        } else {
                            new_row[1, colname] <- input[[colname]]
                        }
                    } else {
                        if (colname %not_in% cols_to_not_create) {
                            shiny::showNotification(
                                paste0(
                                    "The data could not be submitted because ",
                                    colname,
                                    " is not in the database. Please contact the administrator."
                                ),
                                type = "error"
                            )
                            allow_submit(FALSE)
                        }
                    }
                } else if (colname == "data_type") {
                    new_row[1, colname] <- paste(
                        tbl,
                        "(can access other surveys below if needed)"
                    )
                } else if (grepl("other_unspecified", colname)) {
                    # Handle extra taxa inputs
                    other_input_id <- paste0(
                        colname,
                        "-taxa_abundance"
                    )
                    other_taxa_name <- paste0(
                        colname,
                        "-taxa_text"
                    )
                    if (not_null(input[[other_input_id]])) {
                        new_row[1, colname] <- input[[other_input_id]]
                    }
                    if (not_null(input[[other_taxa_name]])) {
                        other_taxa_names <- paste(
                            input[[other_taxa_name]],
                            other_taxa_names,
                            sep = "; "
                        )
                    }
                } else if (colname %not_in% cols_to_not_create) {
                    shiny::showNotification(
                        paste0(
                            colname,
                            " is missing. Please contact the administrator."
                        ),
                        type = "warning"
                    )
                }
            }
            if (allow_submit()) {
                new_row$names_of_other_taxa <- other_taxa_names
                if (ncol(existing_data) == length(new_row)) {
                    # Ensure the new data has the same columns as the existing table
                    names(new_row) <- names(existing_data)
                    DBI::dbWriteTable(
                        con,
                        tbl_name,
                        new_row,
                        append = TRUE
                    )

                    # Put the data in the Google Sheet as well
                    googlesheets4::sheet_append(
                        ss = google_sheet_id,
                        data = as.data.frame(select(new_row, -id)),
                        sheet = tbl
                    )

                    # If all checks pass, show a confirmation notification
                    shiny::showNotification(
                        "Your data has been submitted successfully. Thank you!",
                        type = "message"
                    )
                } else {
                    shiny::showNotification(
                        "The data could not be submitted because the database structure has changed. Please contact the administrator.",
                        type = "error"
                    )
                    allow_submit(FALSE)
                }

                DBI::dbDisconnect(con)
            }
        })
        list(
            values = values,
            submit = shiny::reactive(input$submit)
        )
    })
}
