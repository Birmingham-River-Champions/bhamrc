#' subset_data
#'
#' #' Function to subset data from Google Sheets for the River Champions project
#'
#' This function imports data from Google Sheets and breaks it into several different data types
#' @param input_df Data frame containing the raw data from Google Sheets.
#' @param col_name_start The starting column name for selecting relevant data.
#' @param col_name_end The ending column name for selecting relevant data.
#' @param data_type_name A string indicating the type of data being processed (e.g
#' "Urban Riverfly", "Water Quality", etc.) for warning messages.
#' @return A cleaned data frame ready for analysis.
#' @importFrom dplyr select filter mutate distinct
subset_data <- function(
    input_df,
    col_name_start,
    col_name_end,
    data_type_name
) {
    if (data_type_name == "Water Quality") {
        input_df <- input_df |>
            select(-survey_date) |>
            rename(survey_date = wq_survey_date)
    }
    cleaned_df <- input_df |>
        #First select columns from col_name_start -> col_name_end
        dplyr::select(
            timestamp,
            email_address,
            organisation,
            survey_date,
            data_type,
            !!(col_name_start):!!(col_name_end)
        )

    return(cleaned_df)
}
