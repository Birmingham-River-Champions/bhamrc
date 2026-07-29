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
      start <- Sys.time()

      timings <- list()

      timings$start <- Sys.time()

      # Mirai is a separate process
      # requiring us to read in libraries, variables, etc.
      library(googlesheets4)
      library(dplyr)
      library(sf)
      cat("Packages loaded:", Sys.time(), "\n")

      # Load auth cache
      # Note: cache is not available then you will need
      # to create it using googlesheets4 auth
      # options(gargle_oauth_cache = ".secrets")
      gs4_auth(cache = ".secrets", email = "jamesjrtripp@gmail.com")

      # Read in config options (including google sheet urls, etc.)
      source('R/config.R')

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

      timings$submission_loaded <- Sys.time()

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

      timings$location_loaded <- Sys.time()

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

      timings$finished <- Sys.time()

      # Return results
      # Currently picks two psuedorandomly sampled
      # results for testing purposes
      list(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        timings = timings,
        data = list(
          df_geolocated_submissions = df_geolocated_submissions |> sample_n(2)
        )
      )
    })
  }
}
