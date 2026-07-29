#' Starts an asynchonous job in the background
#' @importFrom mirai mirai
#' @importFrom googlesheets4 gs4_auth read_sheet
#' @importFrom dplyr left_join rename select
#' @noRd
be_start <- function() {
  # If running then return
  if (be$running) {
    return(invisible(FALSE))
  } else {
    # Otherwise run backend
    be$running <- TRUE

    be$job <- mirai({
      # Mirai is a separate process
      # requiring us to read in libraries, variables, etc.
      library(googlesheets4)
      library(dplyr)
      library(sf)
      library(arrow)

      # Load auth cache
      # Note: cache is not available then you will need
      # to create it using googlesheets4 auth
      # options(gargle_oauth_cache = ".secrets")
      gs4_auth(cache = ".secrets", email = "jamesjrtripp@gmail.com")

      # Read in config options (including google sheet urls, etc.)
      source('R/config.R')

      # Read in google data
      submissions <- read_sheet(new_sheet_id)

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
