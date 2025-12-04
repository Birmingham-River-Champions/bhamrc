## Helper functions for plot data processing

#' Flip site names from "Site, Organisation" to "Organisation, Site"
#' @param site_name A character string representing the site name.
#' @return A character string with the site name flipped.
flip_site_names <- function(site_name) {
    # Use regex to capture the two parts of the string
    gsub("^(\\w+(?: \\w+)*),\\s*(.*)$", "\\2, \\1", site_name)
}

#' Anonymise organisations based on sign-up sheet preferences
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

#' Check for a valid email address
#' @param x entered string in the email address field
#' @return boolean whether email address format is correct
isValidEmail <- function(x) {
    grepl(
        "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
        as.character(x),
        ignore.case = TRUE
    )
}
