# Creates environment to store back end variables
# Runs before Shiny process starts so environment is accessible
# to all subsequently created Shiny sessions
be <- new.env(parent = emptyenv())

be$running <- FALSE
be$job <- NULL
be$timestamp <- "Never"
be$data <- NULL
be$run <- 0

be$started <- FALSE


#' Starts an asynchonous job in the background
#' @importFrom mirai mirai
#' @importFrom googlesheets4 gs4_auth read_sheet
#' @importFrom dplyr left_join rename select
#' @importFrom lgr get_logger AppenderFile
#' @noRd
be_start <- function() {
  # If running then return
  if (be$running) {
    return(invisible(FALSE))
  } else {
    # Otherwise run backend
    be$running <- TRUE

    be$job <- mirai({
      library(lgr)
      # setup logger
      lg <- get_logger('bhamrc')

      # Set log location
      lg$set_appenders(
        AppenderFile$new("logs/bhamrc.log")
      )

      lg$info('Mirai job started')

      # Mirai is a separate process
      # requiring us to read in libraries, variables, etc.
      library(googlesheets4)
      library(dplyr)
      library(sf)
      lg$info('Packages loaded')

      # Load auth cache
      # Note: cache is not available then you will need
      # to create it using googlesheets4 auth
      # options(gargle_oauth_cache = ".secrets")
      gs4_auth(path = '.secrets/birminghamriverchampions-db5399f61d80.json')

      # Read in config options (including google sheet urls, etc.)
      source('R/config.R')

      withCallingHandlers(
        {
          # Read in google data
          # All sheets are loaded into
          # one table. Helps for later filtering.
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

          lg$info('Submission dataframe downloaded from Google sheets')

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

          lg$info('Locations parsed')

          # Add coordinates to submission information
          coords <- locations |>
            select(LONG, LAT) |>
            st_as_sf(coords = c("LONG", "LAT"), crs = 27700) |>
            st_transform(4326) |>
            st_coordinates()

          geolocations <- cbind(
            locations |> select(sampling_site),
            coords
          )

          df_geolocated_submissions <-
            left_join(
              submissions,
              geolocations,
              by = "sampling_site",
              relationship = "many-to-many"
            )

          lgr$info('Finished loading data')
        },
        message = function(m) {
          lg$info(conditionMessage(m))
          invokeRestart("muffleMessage")
        },
        warning = function(w) {
          lg$warn(conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      # Return results
      # Currently picks two psuedorandomly sampled
      # results for testing purposes
      list(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        data = list(
          df_geolocated_submissions = df_geolocated_submissions |> sample_n(2)
        )
      )
    })
  }
}

# R/be_startup.R
be_startup <- function() {
  mirai::daemons(1)
  # Start polling for completed jobs
  be_poll()

  # Trigger initial refresh immediately
  be_start()

  # Schedule future refreshes
  be_schedule_check(
    interval = 1
  )
}

#' Triggers check of backend job every second
#' @importFrom later later
#' @noRd
be_poll <- function() {
  be_check()

  later(
    be_poll,
    delay = 0.1
  )
}

# Checks if backend job running.
# Once complete moves data to be$data and sets flags
#' @importFrom mirai unresolved
#' @noRd
be_check <- function() {
  # Just return if job is null or unresoved
  # see https://mirai.r-lib.org/reference/unresolved.html
  if (is.null(be$job)) {
    return()
  }

  if (unresolved(be$job)) {
    return()
  }

  # Below code only runs when the mirai job is
  # neither unresolved or not running.

  # pass result to be$data for convenience
  be$timestamp <- be$job$timestamp # Record when run

  be$run <- be$run + 1 #iterate run count

  # Update data value when marai back end job
  # contains data
  if (!is.null(be$job$data)) {
    be$data <- be$job$data
  }

  # Clear mira back end job data and set job running flag to false
  be$job <- NULL
  be$running <- FALSE
}

# Checks if back end job running.
# Once complete moves data to be$data an sets flags
#' @param interval interval between checks
#' @importFrom later later
#' @noRd
be_schedule_check <- function(interval = be_interval) {
  be_start()

  later(
    function() {
      be_schedule_check(interval)
    },
    delay = interval
  )
}
