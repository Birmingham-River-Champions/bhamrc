#' clean_data
#'
#' #' Function to clean and process data from Google Sheets for the River Champions project
#'
#' This function imports data from Google Sheets, breaks it into several different data types,
#' and processes it to prepare for analysis and visualization.
#' @param input_df Data frame containing the raw data from Google Sheets.
#' @param col_name_start The starting column name for selecting relevant data.
#' @param col_name_end The ending column name for selecting relevant data.
#' @param sample_site Column in input_df that is used to filter out data uploads for this specific data type.
#' @param acceptable_site_orgs Data frame containing valid sampling site locations and their associated organizations,
#' used to filter out a row if the sampling site and organization don't match.
#' @param data_type_name A string indicating the type of data being processed (e.g
#' "Urban Riverfly", "Water Quality", etc.) for warning messages.
#' @return A cleaned data frame ready for analysis.
#' @importFrom dplyr select filter mutate distinct
#' @noRd
clean_data <- function(
    input_df,
    col_name_start,
    col_name_end,
    sample_site,
    acceptable_site_orgs,
    data_type_name
) {
    cleaned_df <- input_df |>
        #First select columns from col_name_start -> col_name_end
        dplyr::select(
            organisation,
            survey_date,
            data_type,
            !!(col_name_start):!!(col_name_end)
        ) |>
        ###Remove data uploads that included no site identifier
        dplyr::filter(!(!!sample_site) == "")

    # Filter out any observations for which the sampling site and organisation don't match what is expected
    wrong_org <- cleaned_df |>
        dplyr::mutate(
            site_orgs = paste(organisation, !!as.name(sample_site))
        ) |>
        dplyr::filter(grepl(!!(data_type_name), data_type)) |>
        dplyr::filter(!(site_orgs %in% acceptable_site_orgs$identifiers))

    ## Filter out rows where the sampling site and organisation don't match
    correct_org_df <- cleaned_df |>
        dplyr::mutate(
            site_orgs = paste(organisation, !!as.name(sample_site))
        ) |>
        dplyr::filter(site_orgs %in% acceptable_site_orgs$identifiers) |>
        dplyr::select(-site_orgs)

    #Also check if there are duplicates, each sampling site + timestamp should be unique
    deduped_df <- correct_org_df |>
        dplyr::distinct(
            survey_date,
            !!(as.name(sample_site)),
            .keep_all = TRUE
        )

    if (nrow(deduped_df) != nrow(cleaned_df)) {
        # If any sampling sites have been associated with the wrong organisation, throw an error
        if (nrow(wrong_org) > 0) {
            warning(
                "Warning: Some ",
                data_type_name,
                " sampling sites seem incorrectly labelled: ",
                wrong_org$site_orgs
            )
        } else {
            # Add a new warning to the list if duplicate combinations exist
            warning(
                paste(
                    "Warning: Duplicated",
                    data_type_name,
                    "sample locations / date - check",
                    data_type_name,
                    "_deduped."
                )
            )
        }
    }

    return(deduped_df)
}
