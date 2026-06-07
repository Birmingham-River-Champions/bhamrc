#' Function to read in data from Google Sheets and populate the SQLite database
#' Also saves cleaned data as internal package data
#' @param data_types A character vector specifying which data types to process.
#' @param table_name A character vector specifying the corresponding SQLite table names for each data type.
#' @param col_indices A numeric vector specifying the starting column index for each data type in the Google Sheet.
#' Options include "riverfly", "water_quality", "invasive_species", and "
#' outfall_safari". Default is to process all data types.
#' @return None. The function creates/updates the SQLite database and saves cleaned data as internal package data.
#' @importFrom googlesheets4 read_sheet
#' @importFrom dplyr select mutate filter distinct case_when
#' @importFrom lubridate dmy years
turn_newsheet_into_db <- function(
    data_types = c(
        "Urban Riverfly",
        "Water Quality",
        "Urban Outfall Safari",
        "Invasive Species"
    ),
    table_name = c(
        "riverfly",
        "water_quality",
        "outfall_safari",
        "invasive_species"
    ),
    col_indices = c(6, 6, 7, 7)
) {
    # Function to create the SQLite database and tables if they don't exist
    sheet_url <- "https://docs.google.com/spreadsheets/d/1jRIIBVBYvEJNkgIcEqnOmFHn4bw7Syimw_4Ad3lV7XY/edit?usp=sharing"

    # Create location data frames for the two different location tables
    locations_list <- process_locations(
        sampling_locs_url = 'https://docs.google.com/spreadsheets/d/1ZEkLC3HBkB8SJynA3pHtmntMOiCT8p4e2BFNYsMUR4c/edit?usp=sharing',
        outfall_locs_url = 'https://docs.google.com/spreadsheets/d/1JJ8bPWppVKbmCfllIevrVmt_dcoswOim7Cos418Ot6w/edit?gid=0#gid=0'
    )

    column_names <- column_types <- vector("list", length(data_types))

    generic_column_names = c(
        "timestamp",
        "email_address",
        "organisation",
        "survey_date",
        "data_type"
    )

    column_names[[1]] <- c(
        generic_column_names,
        c(
            "sampling_site",
            "cased_caddisfly",
            "caseless_caddisfly",
            "olive_mayfly",
            "blue_winged_olive_mayfly",
            "freshwater_shrimp",
            "freshwater_hoglouse",
            "blackfly_larvae",
            "freshwater_worm",
            "freshwater_leech",
            "freshwater_snail",
            "freshwater_beetle",
            "green_drake_mayfly",
            "flat_bodied_stone_clinger_mayfly",
            "stonefly_plecoptera",
            "other_chironomidae",
            "other_dicranota",
            "other_tipulidae",
            "other_hydracarina",
            "other_hydropsychidae",
            "other_rhyacophilidae",
            "other_planorbidae",
            "other_sphaeriidae",
            "other_acroloxidae_ancylidae",
            "other_bullhead",
            "other_unspecified_1",
            "other_unspecified_2",
            "other_unspecified_3",
            "other_unspecified_4",
            "other_unspecified_5",
            "other_unspecified_6",
            "other_unspecified_7",
            "other_unspecified_8",
            "names_of_other_taxa"
        )
    )

    column_names[[2]] <- c(
        generic_column_names,
        c(
            "sampling_site",
            "conductivity_mS",
            "temperature_C",
            "ammonia_ppm",
            "phosphate_ppm",
            "nitrate_ppm",
            "turbidity_NTU",
            "other_water_quality"
        )
    )
    column_names[[3]] <- c(
        generic_column_names,
        c(
            "outfall_survey_date",
            "sampling_site",
            "outfall_photo",
            "outfall_flow",
            "outfall_pollution_distance",
            "outfall_aesthetics",
            "other_pollution_description"
        )
    )

    column_names[[4]] <- c(
        generic_column_names,
        c(
            "invasive_spp_sampling_date",
            "sampling_site",
            "invasive_spp_wtw",
            "signal_crayfish",
            "killer_demon_shrimp",
            "himalayan_balsam",
            "japanese_knotweed",
            "giant_hogweed",
            "any_other_invasive_spp"
        )
    )

    column_types[[1]] <- paste(
        rep("c", length(column_names[[1]])),
        collapse = ""
    )

    column_types[[2]] <- paste(
        c(rep("c", 6), rep("n", 6), rep("c", 1)),
        collapse = ""
    )

    column_types[[3]] <- paste(
        rep("c", length(column_names[[3]])),
        collapse = ""
    )

    column_types[[4]] <- paste(
        rep("c", length(column_names[[4]])),
        collapse = ""
    )

    for (i in seq_len(length(data_types))) {
        if (
            length(column_names[[i]]) != stringr::str_length(column_types[[i]])
        ) {
            stop(paste0(
                "Length of column names and column types must be the same for data type: ",
                data_types[i]
            ))
        }
        if (data_types[i] != "") {
            sub_table <- as.data.frame(
                googlesheets4::read_sheet(
                    sheet_url, # Get rid of duplicate columns, spaces, and other odd characters in column names
                    sheet = data_types[i],
                    col_types = column_types[[i]],
                    col_names = column_names[[i]]
                )
            )

            # Replace "N/A" with blank values
            sub_table <- sub_table |>
                mutate(across(everything(), ~ replace(., . == "N/A", "")))

            locations_name <- c(
                "riverfly_locs",
                "riverfly_locs",
                "outfall_locs",
                "riverfly_locs"
            )

            sub_table <- sub_table |>
                clean_data(
                    sample_site = "sampling_site",
                    locations_name = case_when(
                        data_types[i] ==
                            "Urban Outfall Safari" ~ "outfall_locs",
                        .default = "riverfly_locs"
                    ),
                    data_type_name = data_types[i]
                )
            #db_create_and_pop(
            #    sub_table,
            #    locations_name[i],
            #    data_types[i],
            #    col_indices[i],
            #    table_name = table_name[i]
            #)
        }
    }
}
