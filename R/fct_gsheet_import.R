## code to prepare `data_20250922` dataset goes here
#' Function to read in data from Google Sheets and populate the SQLite database
#' Also saves cleaned data as internal package data
#' @param data_types A character vector specifying which data types to process.
#' Options include "riverfly", "water_quality", "invasive_species", and "
#' outfall_safari". Default is to process all data types.
#' @return None. The function creates/updates the SQLite database and saves cleaned data as internal package data.
#' @importFrom gsheet gsheet2tbl
#' @importFrom dplyr select mutate filter distinct case_when
#' @importFrom lubridate dmy years
#' @noRd
turn_gsheets_into_db <- function(
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
    col_indices = c(4, 4, 5, 5)
) {
    # Function to create the SQLite database and tables if they don't exist
    full_form_url <- "https://docs.google.com/spreadsheets/d/1458OWr2_x3vdM_LGAQaf0lcOWitO9LtnRm2GsAF_pys/edit?usp=sharing"
    BRC_full_form <- as.data.frame(gsheet::gsheet2tbl(full_form_url))

    # Get rid of duplicate columns, spaces, and other odd characters in column names
    names(BRC_full_form) <- c(
        "timestamp",
        "email_address",
        "organisation",
        "data_type",
        "sampling_site_riverfly",
        "survey_date",
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
        "names_of_other_taxa",
        "second_data_type",
        "invasive_spp_sampling_date",
        "invasive_spp_sampling_site",
        "invasive_spp_what_three_words",
        "signal_crayfish",
        "killer_demon_shrimp",
        "himalayan_balsam",
        "japanese_knotweed",
        "giant_hogweed",
        "any_other_invasive_spp",
        "third_data_type",
        "wq_survey_date",
        "wq_sampling_site",
        "conductivity_mS",
        "temperature_C",
        "ammonia_ppm",
        "phosphate_ppm",
        "nitrate_ppm",
        "turbidity_NTU",
        "other_water_quality",
        "fourth_data_type",
        "outfall_survey_date",
        "outfall_sampling_site",
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

    BRC_full_form <- BRC_full_form |>
        mutate(across(everything(), ~ replace(., . == "N/A", "")))

    # Create location data frames for the two different location tables
    locations_list <- process_locations(
        sampling_locs_url = 'https://docs.google.com/spreadsheets/d/1ZEkLC3HBkB8SJynA3pHtmntMOiCT8p4e2BFNYsMUR4c/edit?usp=sharing',
        outfall_locs_url = 'https://docs.google.com/spreadsheets/d/1JJ8bPWppVKbmCfllIevrVmt_dcoswOim7Cos418Ot6w/edit?gid=0#gid=0'
    )

    locations_name <- c(
        "riverfly_locs",
        "riverfly_locs",
        "outfall_locs",
        "riverfly_locs"
    )

    # Create the database tables if they don't exist
    for (i in seq_len(length(data_types))) {
        if (data_types[i] != "") {
            db_create_and_pop(
                BRC_full_form,
                locations_name[i],
                data_types[i],
                col_indices[i],
                table_name = table_name[i]
            )
        }
    }
}

# Read in data from Google Sheets
#' db_create
#' A function to create a SQLite database with specified tables for storing Riverfly and Water Quality data.
#' @param BRC_full The full data frame read in from the Google Sheet.
#' @param data_type A string indicating the type of data being processed (e.g., "Urban Riverfly", "Water Quality", etc.).
#' @param index_of_site_col The index of the column in the cleaned data that contains the sampling site information.
#' @param table_name Name of the SQLite table file to create.
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr case_when
db_create_and_pop <- function(
    full_form,
    locations_list,
    data_type,
    index_of_site_col,
    table_name,
    ...
) {
    processed_data <- clean_data(
        input_df = full_form,
        col_name_start = case_when(
            data_type == "Urban Riverfly" ~ "data_type",
            data_type == "Water Quality" ~ "wq_sampling_site",
            data_type == "Invasive Species" ~ "invasive_spp_sampling_date",
            data_type == "Urban Outfall Safari" ~ "outfall_survey_date"
        ),
        col_name_end = case_when(
            data_type == "Urban Riverfly" ~ "other_bullhead",
            data_type == "Water Quality" ~ "turbidity_NTU",
            data_type == "Invasive Species" ~ "any_other_invasive_spp",
            data_type == "Urban Outfall Safari" ~ "outfall_aesthetics"
        ),
        sample_site = case_when(
            data_type == "Urban Riverfly" ~ "sampling_site_riverfly",
            data_type == "Water Quality" ~ "wq_sampling_site",
            data_type == "Invasive Species" ~ "invasive_spp_sampling_site",
            data_type == "Urban Outfall Safari" ~ "outfall_sampling_site"
        ),
        locations_name = case_when(
            data_type == "Urban Outfall Safari" ~ "outfall_locs",
            .default = "riverfly_locs"
        ),
        data_type_name = data_type
    )

    names(processed_data)[index_of_site_col] <- "sampling_site"

    # Create SQLite tables for riverfly, water quality, and associated location identifiers
    # Invasive species and outfall safari data not currently being added to the database because they are empty
    db_create(table_name, ...)

    # Populate the database tables with the cleaned data
    populate_db(processed_data, table_name)
}

# usethis::use_data(data_20250922, overwrite = TRUE)
