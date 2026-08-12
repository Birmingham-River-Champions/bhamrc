get_locations <- function(sampling_locations_url, outfall_locations_url) {
  # Download locations
  locations <- rbind(
    read_sheet(sampling_locations_url) |>
      rename(
        sampling_site = `BRC sampling site ID`,
        LONG = Easting,
        LAT = Northing
      ),

    read_sheet(outfall_locations_url) |>
      rename(
        sampling_site = `Outfall ID`,
        LONG = Easting,
        LAT = Northing
      )
  )

  # Convert geospatial long and lat
  coords <- locations |>
    select(LONG, LAT) |>
    st_as_sf(coords = c("LONG", "LAT"), crs = 27700) |>
    st_transform(4326) |>
    st_coordinates()

  # Combine locations and long/lat
  geolocations <- cbind(
    locations,
    coords
  )

  # Remove origonal easting and westing
  # I confusingly called these LONG and LAT
  # in the dataframe
  geolocations <- geolocations |>
    select(-LONG, -LAT) |>
    rename(LONG = "X", LAT = "Y")

  # Return location data
  return(geolocations)
}
