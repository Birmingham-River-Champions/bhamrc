#' Clean and validate submission records
#'
#' Validates submitted survey records against a reference table of valid
#' organisation and sampling-site combinations, removes invalid records,
#' and deduplicates submissions based on survey date and sampling site.
#'
#' @param submissions A data frame containing survey submissions.
#'   Expected columns include:
#'   \code{organisation}, \code{sampling_site},
#'   \code{survey_date}, \code{data_type}, and \code{dataset}.
#'
#' @param locations A data frame containing valid organisation/site mappings.
#'   Expected columns include:
#'   \code{Organisations} and \code{sampling_site}.
#'
#' @return A cleaned data frame containing only valid, deduplicated records.
#'
#' @examples
#' \dontrun{
#' cleaned_submissions <- clean_submissions(
#'   submissions = submissions_df,
#'   locations = locations_df
#' )
#' }
clean_submissions <- function(submissions, locations) {
  # Create a lookup of valid organisation/sampling-site combinations.
  acceptable_site_orgs <- locations |>
    mutate(
      identifier = paste(Organisation, sampling_site)
    ) |>
    pull(identifier)

  # Create a matching identifier in the submissions dataset.
  submissions_with_ids <- submissions |>
    mutate(
      site_orgs = paste(organisation, sampling_site)
    )

  # Identify submissions whose organisation/site combination is not present
  # in the reference lookup table.
  wrong_org_df <- submissions_with_ids |>
    filter(!site_orgs %in% acceptable_site_orgs)

  # Retain only submissions with valid organisation/site combinations.
  correct_org_df <- submissions_with_ids |>
    filter(site_orgs %in% acceptable_site_orgs) |>
    select(-site_orgs)

  # Remove duplicate survey entries for the same sampling site and date.
  # The first record encountered is retained.
  deduped_df <- correct_org_df |>
    distinct(
      survey_date,
      sampling_site,
      .keep_all = TRUE
    )

  # Generate warning if invalid organisation/site combinations were found.
  if (nrow(wrong_org_df) > 0) {
    warning(
      paste0(
        "Some organisation/sampling-site combinations are invalid: ",
        paste(unique(wrong_org_df$site_orgs), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Generate warning if duplicate records were removed.
  duplicates_removed <- nrow(correct_org_df) - nrow(deduped_df)

  if (duplicates_removed > 0) {
    warning(
      paste0(
        duplicates_removed,
        " duplicate submission(s) removed based on survey_date and sampling_site."
      ),
      call. = FALSE
    )
  }

  # Return cleaned dataset.
  return(deduped_df)
}
