# A file containing the previous functions
# Placed here whilst moving over to
# new pipeline

#' Function to get the tables from the new Google Sheet
#' @param sheet_url The URL of the Google Sheet containing the data.
#' @return A list of data frames, each corresponding to a table in the Google Sheet.
#' @importFrom googlesheets4 read_sheet
#' @export
put_table_data <- function(
  sheet_url,
  data_types = c(
    "riverfly",
    "water_quality",
    "outfall_safari",
    "invasive_species"
  )
) {
  for (dt in data_types) {
    table_name <- names(data_types_bw)[data_types_bw == dt]
    each_tbl <- googlesheets4::read_sheet(sheet_url, table_name)
    # Create SQLite tables for riverfly, water quality, and associated location identifiers
    # Invasive species and outfall safari data not currently being added to the database because they are empty
    db_create(dt)

    # Populate the database tables with the cleaned data
    populate_db(each_tbl, dt)
  }

  return(table_list)
}

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
  sheet_url <- new_sheet_id

  # Create location data frames for the two different location tables
  locations_list <- process_locations(
    sampling_locs_url = sampling_locations_url,
    outfall_locs_url = outfall_locations_url
  )

  column_names <- column_types <- vector("list", length(data_types))

  generic_column_names <- c(
    "timestamp",
    "email_address",
    "organisation",
    "survey_date",
    "data_type"
  )

  column_names[[1]] <- c(
    generic_column_names,
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

  column_names[[2]] <- c(
    generic_column_names,
    c(
      "sampling_site",
      "conductivity_mS",
      "temperature_C",
      "ammonia_ppm",
      "phosphate_ppm",
      "nitrate_ppm",
      "turbidity_NTU",
      "other_water_quality"
    )
  )
  column_names[[3]] <- c(
    generic_column_names,
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

  column_names[[4]] <- c(
    generic_column_names,
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

  column_types[[1]] <- paste(
    rep("c", length(column_names[[1]])),
    collapse = ""
  )

  column_types[[2]] <- paste(
    c(rep("c", 6), rep("n", 6), rep("c", 1)),
    collapse = ""
  )

  column_types[[3]] <- paste(
    rep("c", length(column_names[[3]])),
    collapse = ""
  )

  column_types[[4]] <- paste(
    rep("c", length(column_names[[4]])),
    collapse = ""
  )

  for (i in seq_len(length(data_types))) {
    if (length(column_names[[i]]) != stringr::str_length(column_types[[i]])) {
      stop(paste0(
        "Length of column names and column types must be the same for data type: ",
        data_types[i]
      ))
    }

    if (data_types[i] != "") {
      sub_table <- as.data.frame(
        googlesheets4::read_sheet(
          sheet_url, # Get rid of duplicate columns, spaces, and other odd characters in column names
          sheet = data_types[i],
          col_types = column_types[[i]],
          col_names = column_names[[i]]
        )
      )

      # Replace "N/A" with blank values
      sub_table <- sub_table |>
        mutate(across(everything(), ~ replace(., . == "N/A", "")))

      sub_table <- sub_table |>
        clean_data(
          sample_site = "sampling_site",
          locations_name = case_when(
            data_types[i] == "Urban Outfall Safari" ~ "outfall_locs",
            .default = "riverfly_locs"
          ),
          data_type_name = data_types[i]
        )
      db_create_and_pop(
        sub_table,
        index_of_site_col = col_indices[i],
        table_name = table_name[i]
      )
    }
  }
}

#' process_locations
#'
#' @description Processes the locations from the Google Sheets and creates location data frames
#'
#' @param sampling_locs_url URL of the Google Sheet containing sampling locations.
#' @param outfall_locs_url URL of the Google Sheet containing outfall locations.
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr bind_cols rename rename_with select contains
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom googlesheets4 read_sheet
#' @export
process_locations <- function(
  sampling_locs_url = 'https://docs.google.com/spreadsheets/d/1ZEkLC3HBkB8SJynA3pHtmntMOiCT8p4e2BFNYsMUR4c/edit?usp=sharing',
  outfall_locs_url = 'https://docs.google.com/spreadsheets/d/1JJ8bPWppVKbmCfllIevrVmt_dcoswOim7Cos418Ot6w/edit?gid=0#gid=0'
) {
  #Then bring in the locations of all of the known eco/WQ sampling points - BRC project team can be used for any site
  BRC_Sampling_Locs_raw <- as.data.frame(googlesheets4::read_sheet(
    sampling_locs_url
  ))
  #And the same for Urban Outfall Safari locations - BRC project team can be used for any site
  BRC_Outfall_Locs_raw <- as.data.frame(googlesheets4::read_sheet(
    outfall_locs_url
  ))
  ##Was going to do invasive species separate with what3words, but code was a nightmare. Use BRC sampling sites, and in future
  ##convert what3words manually on grid reference finder and then label this as something like "Sighting 'out and about'"

  ####Now convert the site IDs into geospatial form (converting east/northing to lat/long)
  BRC_Sampling_Locs_sf <- sf::st_as_sf(
    BRC_Sampling_Locs_raw,
    coords = c("Easting", "Northing"),
    crs = 27700
  ) |>
    sf::st_transform(crs = 4326) |>
    sf::st_coordinates()

  BRC_Sampling_Locs <- BRC_Sampling_Locs_raw |>
    dplyr::bind_cols(data.frame(BRC_Sampling_Locs_sf)[, 2]) |>
    dplyr::bind_cols(data.frame(BRC_Sampling_Locs_sf)[, 1]) |>
    dplyr::rename(
      LAT = ...5,
      LONG = ...6
    ) |>
    dplyr::rename_with(
      ~ return("sampling_site"),
      contains(c("BRC.sampling.site.ID", "BRC sampling site ID"))
    )

  ###Same for Outfall locs
  BRC_Outfall_Locs_sf <-
    sf::st_as_sf(
      BRC_Outfall_Locs_raw,
      coords = c("Easting", "Northing"),
      crs = 27700
    ) |>
    sf::st_transform(crs = 4326) |>
    sf::st_coordinates()

  BRC_Outfall_Locs <- BRC_Outfall_Locs_raw |>
    dplyr::bind_cols(data.frame(BRC_Outfall_Locs_sf)[, 2]) |>
    dplyr::bind_cols(data.frame(BRC_Outfall_Locs_sf)[, 1]) |>
    dplyr::rename(LAT = ...5, LONG = ...6) |>
    dplyr::rename_with(
      ~ return("sampling_site"),
      contains(c("Outfall.ID", "Outfall ID"))
    )

  db_create("riverfly_locs")
  db_create("outfall_locs")
  # Populate the database tables with the cleaned data
  populate_db(BRC_Sampling_Locs, "riverfly_locs")
  populate_db(BRC_Outfall_Locs, "outfall_locs")

  return(list(BRC = BRC_Sampling_Locs, Outfall = BRC_Outfall_Locs))
}


# Helper function to create acceptable location identifiers
acceptable_locs <- function(df) {
  df |>
    dplyr::mutate(identifiers = paste(Organisation, sampling_site)) |>
    dplyr::select(identifiers)
}

#' populate_db
#'
#' @description A fct function
#' @param data_to_insert A data frame containing the data to be inserted into the database.
#' @param table_name Name of the SQLite table where data should be inserted.
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#'
#' @return The return value, if any, from executing the function.
populate_db <- function(data_to_insert, table_name) {
  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), "data.sqlite", extended_types = TRUE)

  # Insert data into the table_name table
  # Assuming `data_to_insert` is a data frame with the appropriate columns
  dbAppendTable(
    conn = con,
    name = table_name,
    value = data.frame(data_to_insert)
  )

  # Disconnect from the database
  dbDisconnect(con)
}

#' db_create
#'
#' @description A function to create a SQLite database with specified tables for storing Riverfly and Water Quality data.
#' @param table_name Name of the SQLite table file to create.
#' @param db_path Path to the SQLite database file.
#' @return The return value, if any, from executing the function.
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbExistsTable
#' @importFrom RSQLite SQLite
#' @noRd
db_create <- function(table_name = "riverfly", db_path = "data.sqlite") {
  # Create a unique table for each data type
  sql_string <- switch(
    table_name,
    "riverfly" = paste(
      "CREATE TABLE",
      table_name,
      "(id INTEGER PRIMARY KEY, email_address TEXT, timestamp TEXT, organisation TEXT, survey_date TEXT, data_type TEXT,
            sampling_site TEXT, cased_caddisfly TEXT, caseless_caddisfly TEXT,
            olive_mayfly TEXT, blue_winged_olive_mayfly TEXT,
            freshwater_shrimp TEXT, freshwater_hoglouse TEXT, blackfly_larvae TEXT, 
            freshwater_worm TEXT, freshwater_leech TEXT, freshwater_snail TEXT, 
            freshwater_beetle TEXT, green_drake_mayfly TEXT, flat_bodied_stone_clinger_mayfly TEXT,
            stonefly_plecoptera TEXT, other_chironomidae TEXT, other_dicranota TEXT,
            other_tipulidae TEXT, other_hydracarina TEXT, other_hydropsychidae TEXT,
            other_rhyacophilidae TEXT, other_planorbidae TEXT, other_sphaeriidae TEXT,
            other_acroloxidae_ancylidae TEXT, other_bullhead TEXT, other_unspecified_1 TEXT,
            other_unspecified_2 TEXT, other_unspecified_3 TEXT, other_unspecified_4 TEXT, other_unspecified_5 TEXT,
            other_unspecified_6 TEXT, other_unspecified_7 TEXT, other_unspecified_8 TEXT, names_of_other_taxa TEXT)"
    ),
    "water_quality" = paste(
      "CREATE TABLE",
      table_name,
      "(id INTEGER PRIMARY KEY, email_address TEXT, timestamp TEXT, organisation TEXT, survey_date TEXT, data_type TEXT, sampling_site TEXT, 
            conductivity_mS REAL, temperature_C REAL, ammonia_ppm REAL, phosphate_ppm REAL, 
            nitrate_ppm REAL, turbidity_NTU REAL, other_water_quality TEXT)"
    ),
    "riverfly_locs" = paste(
      "CREATE TABLE",
      table_name,
      "(id INTEGER PRIMARY KEY, sampling_site TEXT, Organisation TEXT, Easting INTEGER, Northing INTEGER,
            LAT REAL, LONG REAL)"
    ),
    "invasive_species" = paste(
      "CREATE TABLE",
      table_name,
      "(id INTEGER PRIMARY KEY, email_address TEXT, timestamp TEXT, organisation TEXT, survey_date TEXT, data_type TEXT, invasive_spp_sampling_date TEXT,
            sampling_site TEXT,
            invasive_spp_wtw TEXT, signal_crayfish TEXT, killer_demon_shrimp TEXT,
            himalayan_balsam TEXT, japanese_knotweed TEXT, giant_hogweed TEXT,
            any_other_invasive_spp TEXT)"
    ),
    "outfall_safari" = paste(
      "CREATE TABLE",
      table_name,
      "(id INTEGER PRIMARY KEY, email_address TEXT, timestamp TEXT, organisation TEXT, survey_date TEXT, data_type TEXT, outfall_survey_date TEXT,
            sampling_site TEXT, outfall_photo TEXT,
            outfall_flow TEXT, outfall_pollution_distance TEXT, outfall_aesthetics TEXT, other_pollution_description TEXT)"
    ),
    "riverflytest" = paste(
      "CREATE TABLE",
      table_name,
      "(id INTEGER PRIMARY KEY, email_address TEXT, timestamp TEXT, organisation TEXT, survey_date TEXT, data_type TEXT,
            sampling_site TEXT, cased_caddisfly TEXT, caseless_caddisfly TEXT,
            olive_mayfly TEXT, blue_winged_olive_mayfly TEXT,
            freshwater_shrimp TEXT, freshwater_hoglouse TEXT, blackfly_larvae TEXT, 
            freshwater_worm TEXT, freshwater_leech TEXT, freshwater_snail TEXT, 
            freshwater_beetle TEXT, green_drake_mayfly TEXT, flat_bodied_stone_clinger_mayfly TEXT,
            stonefly_plecoptera TEXT, other_chironomidae TEXT, other_dicranota TEXT,
            other_tipulidae TEXT, other_hydracarina TEXT, other_hydropsychidae TEXT,
            other_rhyacophilidae TEXT, other_planorbidae TEXT, other_sphaeriidae TEXT,
            other_acroloxidae_ancylidae TEXT, other_bullhead TEXT)"
    ),
    "outfall_locs" = paste(
      "CREATE TABLE",
      table_name,
      "(id INTEGER PRIMARY KEY, Organisation TEXT, sampling_site TEXT, Easting INTEGER, Northing INTEGER,
            LAT REAL, LONG REAL)"
    ),
    # Default case if no match is found
    stop("Unknown table name")
  )

  # Connect to the SQLite database (or create it if it doesn't exist)
  # Create a new table if it doesn't already exist
  con <- dbConnect(RSQLite::SQLite(), "data.sqlite", extended_types = TRUE)
  if (!dbExistsTable(con, table_name)) {
    dbExecute(con, sql_string)
  } else {
    message(paste(
      "Table",
      table_name,
      "already exists. Deleting and recreating."
    ))
    dbRemoveTable(con, table_name)
    dbExecute(con, sql_string)
  }

  dbDisconnect(con)
}

# Read in data from Google Sheets
#' db_create
#' A function to create a SQLite database with specified tables for storing Riverfly and Water Quality data.
#' @param sub_table The subsetted data frame read in from the Google Sheet.
#' @param index_of_site_col The index of the column in the cleaned data that contains the sampling site information.
#' @param table_name Name of the SQLite table file to create.
#' @param ... Additional arguments to pass to the db_create function.
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr case_when
db_create_and_pop <- function(
  sub_table,
  index_of_site_col,
  table_name,
  ...
) {
  names(sub_table)[index_of_site_col] <- "sampling_site"

  # Create SQLite tables for riverfly, water quality, and associated location identifiers
  # Invasive species and outfall safari data not currently being added to the database because they are empty
  db_create(table_name, ...)

  # Populate the database tables with the cleaned data
  populate_db(sub_table, table_name)

  # Put the created data into Google Sheets as well
  # googlesheets4::sheet_write(
  #     ss = google_sheet_id,
  #     data = processed_data,
  #     sheet = data_type
  # )
}

#' clean_data
#'
#' #' Function to clean and process data from Google Sheets for the River Champions project
#'
#' This function imports data from Google Sheets, breaks it into several different data types,
#' and processes it to prepare for analysis and visualization.
#' @param cleaned_df Data frame containing the raw data from Google Sheets.
#' @param sample_site Column in input_df that is used to filter out data uploads for this specific data type.
#' @param locations_name The name of the locations table in the SQLite database.
#' @param data_type_name A string indicating the type of data being processed (e.g
#' "Urban Riverfly", "Water Quality", etc.) for warning messages.
#' @return A cleaned data frame ready for analysis.
#' @importFrom dplyr select filter mutate distinct
clean_data <- function(
  cleaned_df,
  sample_site,
  locations_name,
  data_type_name
) {
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    "data.sqlite",
    extended_types = TRUE
  )
  locations <- DBI::dbReadTable(con, locations_name)
  dbDisconnect(con)

  acceptable_site_orgs <- acceptable_locs(locations)

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
