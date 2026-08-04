#' Read and combine all submission datasets from a Google Sheet
#'
#' Reads submission data from the four Urban River LAB Google Sheets
#' ("Urban Riverfly", "Urban Outfall Safari", "Water Quality", and
#' "Invasive Species"), applies predefined column names and column types,
#' standardises known data quality issues, and combines all records into a
#' single data frame.
#'
#' The function includes special handling for the Water Quality sheet, where
#' timestamps may be imported in an inconsistent format and the
#' `timestamp` and `email_address` columns are occasionally transposed.
#'
#' @param submissions_url Character scalar giving the URL or spreadsheet ID of
#'   the Google Sheet containing the submission data.
#'
#' @return A tibble containing records from all submission sheets with a
#'   standardised structure. An additional `dataset` column identifies the
#'   source sheet for each record.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Reads data from the Urban Riverfly, Urban Outfall Safari,
#'     Water Quality, and Invasive Species sheets.
#'   \item Applies predefined column names and column type specifications.
#'   \item Detects and corrects rows where `timestamp` and `email_address`
#'     have been swapped.
#'   \item Combines all submission datasets into a single tibble using
#'     \code{dplyr::bind_rows()}.
#' }
#'
#' @seealso
#' \code{\link[googlesheets4]{read_sheet}}
#'
#' @examples
#' \dontrun{
#' submissions <- get_submissions(
#'   "https://docs.google.com/spreadsheets/d/xxxxxxxxxxxxxxxx"
#' )
#' }
#'
#' @importFrom googlesheets4 read_sheet
#' @importFrom dplyr bind_rows mutate across select rename relocate case_when
#' @importFrom stringr str_detect
#'
#' @export
get_submissions <- function(submissions_url) {
  # Names at start of all datasets
  generic_col_names <- c(
    "timestamp",
    "email_address",
    "organisation",
    "survey_date",
    "data_type"
  )

  # Column names for each dataset
  col_names_riverfly <- c(
    generic_col_names,
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

  col_types_riverfly <- paste(
    rep("c", length(col_names_riverfly)),
    collapse = ""
  )

  col_names_water_quality <- c(
    generic_col_names,
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

  col_types_water_quality <- paste(
    c(rep("c", 6), rep("n", 6), rep("c", 1)),
    collapse = ""
  )

  col_names_outfall_safari <- c(
    generic_col_names,
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

  col_types_outfall_safari <- paste(
    rep("c", length(col_names_outfall_safari)),
    collapse = ""
  )

  col_names_invasive_species <- c(
    generic_col_names,
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

  col_types_invasive_species <- paste(
    rep("c", length(col_names_invasive_species)),
    collapse = ""
  )

  # Read in dataset and combine into single dataframe
  # Wrapper function for reading google sheets and checks
  read_in_data <- function(url, col_names, col_types, dataset) {
    # add checks here
    read_sheet(
      ss = url,
      sheet = dataset,
      col_names = col_names,
      col_types = col_types
    )
  }

  # In some columns the email and timestamp are swapped
  # here is the function for swapping these columns
  find_and_swap_email_timestamp <- function(df) {
    email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
    df |>
      mutate(
        known_email = case_when(
          str_detect(email_address, email_pattern) ~ email_address,
          str_detect(timestamp, email_pattern) ~ timestamp,
          TRUE ~ NA_character_
        ),
        known_timestamp = case_when(
          str_detect(email_address, email_pattern) ~ timestamp,
          str_detect(timestamp, email_pattern) ~ email_address,
          TRUE ~ NA_character_
        )
      ) |>
      select(-email_address, -timestamp) |>
      rename(
        email_address = known_email,
        timestamp = known_timestamp
      ) |>
      relocate(timestamp, email_address)
  }

  submissions <- bind_rows(
    read_in_data(
      submissions_url,
      col_names_riverfly,
      col_types_riverfly,
      "Urban Riverfly"
    ) |>
      mutate(dataset = "Urban Riverfly"),
    read_in_data(
      submissions_url,
      col_names_outfall_safari,
      col_types_outfall_safari,
      "Urban Outfall Safari"
    ) |>
      mutate(dataset = "Urban Outfall Safari"),
    # Water quality data has different columns
    # Commenting out below and just loading in atm
    # read_in_data(
    #   submissions_url,
    #   col_names_water_quality,
    #   col_types_water_quality,
    #   "Water Quality"
    # ) |>
    # The below produces timestamps which appear to be posix
    # Needs to be converted via as.POSIXct
    read_sheet(submissions_url, "Water Quality") |>
      mutate(across(everything(), as.character)) |>
      find_and_swap_email_timestamp() |>
      mutate(dataset = "Water Quality"),
    read_in_data(
      submissions_url,
      col_names_invasive_species,
      col_types_invasive_species,
      "Invasive Species"
    ) |>
      mutate(dataset = "Invasive Species"),
  )

  # Set NAs to 0 in dataframe

  return(
    submissions
  )
}
