###########################################################################
#  Back end data refesh service
#
# This code manages an asynchronous background process using Mirai.
#
# A single Mirai worker downloads and processes data from google sheets.
# The data is stored in the global 'be' environment which is available
# to all Shiny session.
#
# Access the data within Shiny
# #############################
# Within Shiny you can use the below code to
# convert the data from the Mirai worker into a reactive variable
# be_result <- reactivePoll(
#   # Poll every x milliseconds
#   interval = 100,
#   session = session,

#   # This checks the background task data
#   checkFunc = function() {
#     be$data
#   },

#   # Once there is new data in the
#   # mirai job found by reactivePoll
#   # this data is then assigned to
#   # the be_result reactive variable
#   valueFunc = function() {
#     be$data
#   }
# )
#
# Process workflow
# ################
#
# App Startup (in run_app.R  "onStart = be_startup")
#    V
# be_startup()
#    |
#    +--> Starts Mirai daemon (process)
#    |
#    +--> Starts poll looping (be_poll())
#.   |
#    +--> Runs be_start() straight away
#.                   V
#                 be_start()
#                    V
#               Mirai Worker (separate R process)
#                    |
#                    +-> Data download and processing
#                    |
#                    +-> Returns results as list
#                    V
#                  be$job
#                    V
#                be_check() (run regularly by be_poll())
#                    |
#                    +-> Moves results of Mirai worker to be$data
#                    +-> Increments be run so reactivePoll can pull out results
#                    +-> Clears complete job
#                    V
#            Waits n seconds defined by be_refresh_interval (defined in config.R)
#                    V
#                 be_start() (round we go again)

# Create be environment

be <- new.env(parent = emptyenv())

be$running <- FALSE
be$job <- NULL
be$timestamp <- "Never"
be$data <- NULL
be$run <- 0

# Logging functions
#########################

# If logging is set to TRUE in golem-config.yml
# then we print out logging statements for the
# back end in a file called bhamrc.log

# The below functions check if logging is set to
# TRUE and be_logger sets up the logging
# be_log_info is a convenient wrapper
# which returns invisible null if
# logging is disabled.

# Get the logging config setting
be_logging_enabled <- function() {
  isTRUE(
    get_golem_config("enable_logging")
  )
}

#' @importFrom lgr get_logger AppenderFile
#' @noRd
be_logger <- function() {
  if (!be_logging_enabled()) {
    return(NULL)
  }

  lg <- get_logger("bhamrc")

  lg$set_appenders(
    AppenderFile$new(
      "bhamrc.log"
    )
  )

  # Prevent logging to console
  lg$set_propagate(FALSE)

  lg
}

#' @noRd
be_log_info <- function(msg) {
  lg <- be_logger()

  if (!is.null(lg)) {
    lg$info(msg)
  }

  invisible(NULL)
}

# Back end functions
########################

#' Starts an asynchronous mirai back end
#'
#' @importFrom mirai mirai
#' @importFrom googlesheets4 gs4_auth read_sheet
#' @importFrom dplyr left_join rename select
#' @importFrom sf st_transform st_read
#' @importFrom lgr be_logger
#' @noRd
be_start <- function() {
  if (be$running) {
    return(invisible(FALSE))
  }

  enable_logging <- isTRUE(
    get_golem_config("enable_logging")
  )

  library(sf)

  # Read in spatial data which we assume will not update dynamically
  shp_tame <- st_read(
    "./inst/extdata/Upper_Tame_Wbs_Complete_SubCtchmnts_Dsslvd.shp"
  ) |>
    st_transform(crs = 4326)

  shp_tame_river <- st_read(
    "./inst/extdata/Tame_OS_WatercourseLink.shp"
  ) |>
    st_zm(shp_tame) |>
    st_transform(crs = 4326)

  be$running <- TRUE

  be_log_info("Starting Mirai job")

  be$job <- mirai::mirai(
    {
      # Helper functions
      source("R/small_helpers.R")

      # Config variables
      source("R/config.R")

      # Columns for filtering large and wide submission data
      source("R/get_relevent_dataset_columns.R")

      # Download data from google sheets
      source("R/get_submissions.R")
      source("R/get_locations.R")

      # Submission cleaning and checking
      source('R/clean_submissions.R')

      # Create ARMI values and plots
      source('R/make_riverfly_ARMI.R')
      source("R/sum_up_ARMI.r")
      source("R/create_armi_plots.R")
      source('R/make_ARMI_plot_data.r')

      # Water quality
      source("R/make_water_quality_plot_data.r")

      # Species
      source("R/make_species_plots.r")

      # Invasive Species plot
      source("R/make_recent_inv_spp.R")

      # Required libraries sourced here
      # Mirai worker is a seperate process
      library(lgr)
      library(googlesheets4)
      library(dplyr)
      library(sf)
      library(stringr)
      library(ggplot2)
      library(lubridate)
      library(tidyr)
      library(RColorBrewer)

      log_info <- function(msg) {
        if (enable_logging) {
          lg$info(msg)
        }
      }

      lg <- get_logger("bhamrc")

      lg$set_appenders(
        AppenderFile$new(
          "bhamrc.log"
        )
      )

      log_info("Mirai job started")

      gs4_auth(
        path = ".secrets/birminghamriverchampions-db5399f61d80.json"
      )

      out <- withCallingHandlers(
        {
          # Download the current submissions dataset
          submissions <- get_submissions(new_sheet_id)

          log_info("Submission dataframe downloaded")

          # Download and convert location information
          locations <- get_locations(
            sampling_locations_url,
            outfall_locations_url
          )

          log_info("Locations parsed")

          # Refine the submission data
          submissions <- clean_submissions(submissions, locations)

          log_info("Cleaned submissions df")

          log_info("Finished loading data")

          # Combine submission data and corresponding geospatial
          # data for plotting on map
          df_geolocated_submissions <-
            left_join(
              submissions,
              locations,
              by = "sampling_site",
              relationship = "many-to-many"
            )

          # All plot objects are made using the below
          # Should note that these ggplot2 objects are
          # not rendered by like lists which contain the
          # plot data.

          # # All Riverfly plots
          BRC_locs <- locations

          Unique_BRC_Sampling_Locs <-
            BRC_locs |>
            distinct(sampling_site, .keep_all = TRUE)

          riverfly_data <- df_geolocated_submissions |>
            filter(dataset == "Urban Riverfly") |>
            select(any_of(riverfly_cols))

          # Creates plots in large nested list
          Riverfly_Species_Plot_All <- species_plots(
            riverfly_data,
            Unique_BRC_Sampling_Locs
          )

          # # Pull out each list into named variable
          Riverfly_Species_Plot <- Riverfly_Species_Plot_All[[1]]
          Riverfly_Species_Plot_Recent <- Riverfly_Species_Plot_All[[2]]
          Riverfly_Other_Species_Plot <- Riverfly_Species_Plot_All[[3]]
          Riverfly_Other_Species_Plot_Recent <- Riverfly_Species_Plot_All[[4]]

          # log_info("Riverfly plot data created")

          # # Riverfly ARMI plots
          ARMI_assignment <- make_riverfly_ARMI(riverfly_data)
          ARMI_data <- sum_up_ARMI(ARMI_assignment)
          riverflyARMIDataList <- make_ARMI_plot_data(
            ARMI_data,
            Unique_BRC_Sampling_Locs
          )

          # log_info("Riverfly ARMI plots created")

          # # Water plot data
          # # Create water data quality data as expected by function
          cols <- get_relevant_dataset_columns("Water Quality")
          BRC_wq <- df_geolocated_submissions |>
            filter(dataset == "Water Quality") |>
            select(!c("LONG", "LAT")) |>
            select(any_of(cols))

          WQ_plot_data <- make_water_quality_plot_data(
            BRC_wq,
            Unique_BRC_Sampling_Locs
          )

          # log_info("Water Quality plots created")

          plot_palette <- brewer.pal(n = 9, name = "Blues")

          cols <-
            get_relevant_dataset_columns("Invasive Species")
          BRCInvSpcs <- df_geolocated_submissions |>
            filter(dataset == "Invasive Species") |>
            select(!c("LONG", "LAT")) |>
            select(any_of(cols))

          BRCINvSpcs_Plot_Recent <- make_recent_inv_spp(
            BRCInvSpcs,
            BRC_locs,
            plot_palette
          )

          log_info("Invasive Species plots created")

          # Return a list of all the variables presented
          # in the web front end.
          # This happens and then the Mirai worker is closed down
          # Note: This list is available via out$variable_name outside
          # this withCallingHandlers
          return(
            list(
              df_geolocated_submissions = df_geolocated_submissions,
              locations = locations,
              BRC_locs = BRC_locs,
              Unique_BRC_Sampling_Locs = Unique_BRC_Sampling_Locs,
              riverfly_data = riverfly_data,
              Riverfly_Species_Plot_All = Riverfly_Species_Plot_All,
              Riverfly_Species_Plot = Riverfly_Species_Plot,
              Riverfly_Species_Plot_Recent = Riverfly_Species_Plot_Recent,
              Riverfly_Other_Species_Plot = Riverfly_Other_Species_Plot,
              Riverfly_Other_Species_Plot_Recent = Riverfly_Other_Species_Plot_Recent,
              ARMI_assignment = ARMI_assignment,
              ARMI_data = ARMI_data,
              riverflyARMIDataList = riverflyARMIDataList,
              BRC_wq = BRC_wq,
              WQ_plot_data = WQ_plot_data,
              BRCInvSpcs = BRCInvSpcs,
              BRCINvSpcs_Plot_Recent = BRCINvSpcs_Plot_Recent
            )
          )
          #return(df_geolocated_submissions)
        },

        message = function(m) {
          log_info(paste("/t", m))
          invokeRestart("muffleMessage")
        },

        warning = function(w) {
          log_info(paste("/t", w))
          invokeRestart("muffleWarning")
        }
      )

      list(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        data = list(
          df_geolocated_submissions = df_geolocated_submissions,
          locations = locations,
          BRC_locs = BRC_locs,
          Unique_BRC_Sampling_Locs = Unique_BRC_Sampling_Locs,
          riverfly_data = riverfly_data,
          Riverfly_Species_Plot_All = Riverfly_Species_Plot_All,
          Riverfly_Species_Plot = Riverfly_Species_Plot,
          Riverfly_Species_Plot_Recent = Riverfly_Species_Plot_Recent,
          Riverfly_Other_Species_Plot = Riverfly_Other_Species_Plot,
          Riverfly_Other_Species_Plot_Recent = Riverfly_Other_Species_Plot_Recent,
          ARMI_assignment = ARMI_assignment,
          ARMI_data = ARMI_data,
          riverflyARMIDataList = riverflyARMIDataList,
          BRC_wq = BRC_wq,
          WQ_plot_data = WQ_plot_data,
          BBCInvSpcs = BBCInvSpcs,
          BRCINvSpcs_Plot_Recent = BRCINvSpcs_Plot_Recent
        )
      )
    },
    enable_logging = enable_logging,
    shp_tame = shp_tame,
    shp_tame_river = shp_tame_river # Pass enable logging flag to Mirai
  )

  invisible(TRUE)
}

#' Start the background refresh service
#'
#' Initialises the Mirai worker process, starts the polling loop used to
#' detect completed jobs, and triggers the first data refresh immediately.
#'
#' This function should be called once when the application starts.
#'
#' @noRd
be_startup <- function() {
  mirai::daemons(1)

  be_log_info("Backend startup")

  # Start checking for completed jobs
  be_poll()

  # Run immediately
  be_start()
}

#' Poll for completed Mirai jobs
#'
#' Checks whether the current background job has completed and schedules
#' the next polling event. Polling continues for the lifetime of the
#' app.
#'
#' The polling frequency is controlled by `be_poll_interval`.
#'
#' @importFrom later later
#' @noRd
be_poll <- function() {
  be_check()

  later::later(
    be_poll,
    delay = be_poll_interval
  )
}

#' Process completed Mirai jobs
#'
#' Examines the current Mirai job and returns immediately if no job exists
#' or the job is still running.
#'
#' When a job has completed, the returned data are copied into the shared
#' backend environment (`be$data`), the refresh counter (`be$run`) is
#' incremented, and the completed job is cleaned up.
#'
#' A new refresh is then scheduled after `be_refresh_interval` seconds,
#' ensuring that the delay is measured from job completion rather than
#' job start.
#'
#' @importFrom mirai unresolved
be_check <- function() {
  if (is.null(be$job)) {
    return()
  }

  if (unresolved(be$job)) {
    return()
  }

  be_log_info("Mirai job resolved")

  be$timestamp <- be$job$timestamp
  be$run <- be$run + 1

  if (!is.null(be$job$data)) {
    be$data <- be$job$data

    be_log_info(
      paste(
        "Updated be$data. Run =",
        be$run
      )
    )
  }

  # Cleanup completed job
  be$job <- NULL
  be$running <- FALSE

  be_log_info(
    paste(
      "Scheduling next refresh in",
      be_refresh_interval,
      "seconds"
    )
  )

  # Wait X seconds AFTER completion before starting again
  later::later(
    be_start,
    delay = be_refresh_interval
  )
}
