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
