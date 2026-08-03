# Function to read in the 4 sheets containing submission information
# When loading the data column names and data types are specified
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

  return(
    submissions
  )
}
