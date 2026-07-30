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
#               Mirai Worker (seperate R process)
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
#                    +-> Incriments be run so reactivePoll can pull out results
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
#' @noRd
be_start <- function() {
  if (be$running) {
    return(invisible(FALSE))
  }

  enable_logging <- isTRUE(
    get_golem_config("enable_logging")
  )

  be$running <- TRUE

  be_log_info("Starting Mirai job")

  be$job <- mirai::mirai(
    {
      source("R/config.R")

      library(lgr)
      library(googlesheets4)
      library(dplyr)
      library(sf)

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

      withCallingHandlers(
        {
          submissions <- bind_rows(
            read_sheet(new_sheet_id, sheet = "Urban Riverfly") |>
              mutate(across(everything(), as.character)) |>
              mutate(sheet = "Urban Riverfly"),

            read_sheet(new_sheet_id, sheet = "Water Quality") |>
              mutate(across(everything(), as.character)) |>
              mutate(sheet = "Water Quality"),

            read_sheet(new_sheet_id, sheet = "Invasive Species") |>
              mutate(across(everything(), as.character)) |>
              mutate(sheet = "Invasive Species"),

            read_sheet(new_sheet_id, sheet = "Urban Outfall Safari") |>
              mutate(across(everything(), as.character)) |>
              mutate(sheet = "Urban Outfall Safari")
          )

          log_info("Submission dataframe downloaded")

          locations <- rbind(
            read_sheet(sampling_locations_url) |>
              rename(
                sampling_site = `BRC sampling site ID`,
                LONG = Easting,
                LAT = Northing
              ) |>
              select(sampling_site, LONG, LAT),

            read_sheet(outfall_locations_url) |>
              rename(
                sampling_site = `Outfall ID`,
                LONG = Easting,
                LAT = Northing
              ) |>
              select(sampling_site, LONG, LAT)
          )

          log_info("Locations parsed")

          coords <- locations |>
            select(LONG, LAT) |>
            st_as_sf(coords = c("LONG", "LAT"), crs = 27700) |>
            st_transform(4326) |>
            st_coordinates()

          geolocations <- cbind(
            locations |>
              select(sampling_site),
            coords
          )

          df_geolocated_submissions <-
            left_join(
              submissions,
              geolocations,
              by = "sampling_site",
              relationship = "many-to-many"
            )

          log_info("Finished loading data")
        },

        # TODO: Consider logging all caught warnings/messages
        message = function(m) {
          invokeRestart("muffleMessage")
        },

        warning = function(w) {
          invokeRestart("muffleWarning")
        }
      )

      list(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        data = list(
          df_geolocated_submissions = df_geolocated_submissions |>
            sample_n(2)
        )
      )
    },
    enable_logging = enable_logging # Pass enable logging flag to Mirai
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
#' @importFro
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
