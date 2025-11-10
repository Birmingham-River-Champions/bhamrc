## Helper functions for plot data processing

#' Flip site names from "Site, Organisation" to "Organisation, Site"
#' @noRd
#' @param site_name A character string representing the site name.
#' @return A character string with the site name flipped.
flip_site_names <- function(site_name) {
    # Use regex to capture the two parts of the string
    gsub("^(\\w+(?: \\w+)*),\\s*(.*)$", "\\2, \\1", site_name)
}

#' Anonymise organisations based on sign-up sheet preferences
#' @noRd
#' @param data_frame A data frame containing an 'organisation' column.
#' @return A data frame with specified organisations anonymised.
anonymise_organisations <- function(data_frame) {
    data_frame$organisation <- ifelse(
        data_frame$organisation == "Friends of Lifford Reservoir",
        "Anonymous",
        data_frame$organisation
    )
    return(data_frame)
}

#' Remove parenthesised organisations from site names
#' @noRd
#' @param data_frame A data frame containing a 'sampling_site' column.
#' @return A data frame with parenthesised organisations removed from site names.
remove_parenthesised_orgs <- function(data_frame) {
    data_frame$sampling_site <- gsub(
        "\\s*\\(.*\\)$",
        "",
        data_frame$sampling_site
    )
    return(data_frame)
}

# Function to add colours based on reading type and breaks
#' @description Add colours to water quality data based on predefined breaks.
#' @noRd
#' @param plot_data_object A data frame containing water quality data with 'reading_type' and 'value' columns.
#' @return A data frame with an additional 'WQ_Plot_Colour' column.
add_colours <- function(
    plot_data_object,
    plot_palette = brewer.pal(5, "Blues")
) {
    # Reshape the breaks to wide format for joining
    wide_breaks <- plot_breaks |>
        tidyr::pivot_wider(
            names_from = bin,
            values_from = bin_breaks,
            names_prefix = "bin_break_"
        )

    obj_return <- plot_data_object |>
        left_join(
            wide_breaks,
            by = join_by(reading_type),
            multiple = "first"
        ) |>
        mutate(
            # Need to better format these first three in terms of ranges, nice colours etc
            WQ_Plot_Colour = case_when(
                value <= bin_break_1 ~
                    plot_palette[1],
                (value > bin_break_1) &
                    (value <= bin_break_2) ~
                    plot_palette[2],
                (value > bin_break_2) &
                    (value <= bin_break_3) ~
                    plot_palette[3],
                (value > bin_break_3) &
                    (value <= bin_break_4) ~
                    plot_palette[4],
                (value > bin_break_4) ~
                    plot_palette[5],
                is.na(value) ~
                    "grey80"
            )
        ) |>
        dplyr::select(-starts_with("bin_break_"))
    return(obj_return)
}
