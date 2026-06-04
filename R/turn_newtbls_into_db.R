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
    full_form_url <- "https://docs.google.com/spreadsheets/d/1jRIIBVBYvEJNkgIcEqnOmFHn4bw7Syimw_4Ad3lV7XY/edit?usp=sharing"

    # Create location data frames for the two different location tables
    locations_list <- process_locations(
        sampling_locs_url = 'https://docs.google.com/spreadsheets/d/1ZEkLC3HBkB8SJynA3pHtmntMOiCT8p4e2BFNYsMUR4c/edit?usp=sharing',
        outfall_locs_url = 'https://docs.google.com/spreadsheets/d/1JJ8bPWppVKbmCfllIevrVmt_dcoswOim7Cos418Ot6w/edit?gid=0#gid=0'
    )

    colnames <- coltypes <- vector("list", length(data_types))

    generic_colnames = c(
        "timestamp",
        "email_address",
        "organisation",
        "survey_date",
        "data_type",
        "sampling_site"
    )

    colnames[[1]] <- c(
        generic_colnames,
        c(
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
    colnames[[4]] <- c(
        generic_colnames,
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

    colnames[[2]] <- c(
        generic_colnames,
        c(
            "conductivity_mS",
            "temperature_C",
            "ammonia_ppm",
            "phosphate_ppm",
            "nitrate_ppm",
            "turbidity_NTU",
            "other_water_quality"
        )
    )
    colnames[[3]] <- c(
        generic_colnames,
        c(
            "outfall_survey_date",
            "sampling_site",
            "outfall_photo",
            "outfall_flow",
            "outfall_pollution_distance",
            "outfall_aesthetics",
            "other_pollution_description",
            "fifth_data_type",
            "submit_or_return",
            "other_comments_river",
            "outfall_location_wtw",
            "location_wtw"
        )
    )

    coltypes[[1]] <- paste(
        rep("c", 39),
        collapse = ""
    )

    coltypes[[2]] <- paste(
        c(rep("c", 6), rep("n", 6), rep("c", 1)),
        collapse = ""
    )

    coltypes[[3]] <- substr(coltypes[[1]], 1, 14)

    coltypes[[4]] <- substr(coltypes[[1]], 1, 12)

    for (i in seq_len(length(data_types))) {
        if (data_types[i] != "") {
            BRC_full_form <- as.data.frame(
                googlesheets4::read_sheet(
                    full_form_url, # Get rid of duplicate columns, spaces, and other odd characters in column names
                    sheet = data_types[i],
                    col_types = coltypes[[i]],
                    col_names = colnames[[i]]
                )
            )

            # Replace "N/A" with blank values
            BRC_full_form <- BRC_full_form |>
                mutate(across(everything(), ~ replace(., . == "N/A", "")))

            locations_name <- c(
                "riverfly_locs",
                "riverfly_locs",
                "outfall_locs",
                "riverfly_locs"
            )
            # db_create_and_pop(
            #     BRC_full_form,
            #     locations_name[i],
            #     data_types[i],
            #     col_indices[i],
            #     table_name = table_name[i]
            # )
        }
    }
}
