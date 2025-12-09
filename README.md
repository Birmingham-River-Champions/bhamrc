# Birmingham River Champions Shiny App
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![coverage](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/nmfs-ost/ghactions4r/refs/heads/badges/coverage-badge.json)](https://github.com/Birmingham-River-Champions/bhamrc/tree/badges)
<!-- badges: end -->

This is an R Shiny app with a lightweight SQLite database and a dynamic input form. Users can submit data, which is stored in the database and displayed in the app.

## Features
- Modern, light-themed UI
- Dynamic input form for Riverfly Champions Data
- Stores submissions in a local SQLite database
- Displays submitted entries in a table

## Install instructions
1. Ensure you have R and the following packages installed:
   - shiny
   - DBI
   - RSQLite
2. Install the package in RStudio:

```r
install.packages("remotes")
remotes::install_github("Birmingham-River-Champions/bhamrc")
library(bhamrc)
```

3. You can launch the application by running:

```r
bhamrc::run_app()
```
4. The app will create a `data.sqlite` file in the project directory for local data storage.

5. To deploy the app to shinyapps.io, run the following lines in RStudio:
```r
rsconnect::deployApp(
  appName = desc::desc_get_field("Package"),
  appTitle = desc::desc_get_field("Package"),
  appFiles = c(
    # Add any additional files unique to your app here.
    "R/",
    "inst/",
    "data/",
    "NAMESPACE",
    "DESCRIPTION",
    "app.R"
  ),
  appId = rsconnect::deployments(".")$appID,
  lint = FALSE,
  forceUpdate = TRUE
)
```

6. To make changes to the package, clone it from GitHub, make changes locally in the files using RStudio or your preferred IDE, and then re-load the package using `devtools::load_all()`. Then follow the application install instructions above to launch and/or deploy the app.

## Usage
- Fill out the form and click "Submit".
- View all submissions in the table on "Submitted Data" tab.

## File Structure
This app follows structural conventions that are part of the `R` package structure (e.g. `DESCRIPTION`), please refer to this [book](https://r-pkgs.org/) for descriptions of any files and folders that are not specific to this app. App-specific files are described below.

### Shiny application components
- `run_app.R`: Main Shiny app file
- `app_ui.R`: Contains the application UI code. This includes the HTML and text content for the landing page and calls the UI functions of the 6 application modules:
- `mod_01_welcome.R`: This module contains the UI function with content for the welcome page tab. The server function is empty.
- `mod_02_data_input.R`: This module contains the UI and server functions for the input data tab. The server function of this module calls `mod_data_entry_form.R`: which do the bulk of the form creation.
- `mod_03_plot_data.R`: This module contains the UI and server functions for the "Your data" tab that users can view plots on. The server function of this module calls many plotting functions described below.
- `mod_04_information.R`: This module contains the UI function for information content on the Information/Resources tab. Each "information card" uses text that can be edited in the file `inst/app/www/text/information.yml`. The server function is empty. 
- `mod_05_show_data.R`: This module contains the UI and server functions to select the different data tables, show their content, download the data, and edit the data. 
- `mod_06_newsletters.R`: This module contains the UI and server functions for the "Newsletters/Reports" tab. The newsletter content is in the file `inst/app/www/text/newsletters.yml`

### Helper R functions
For more information on these functions, refer to the `man` folder, which has an `.Rd` file containing documentation for each function.
- `add_<data type>_markers.R` functions: These functions add the circle markers to the `leaflet` plots for the specified `<data type>`, one of ARMI, invasive species, other species or water quality data.
- `add_polygons_and_lines.R`: this function draws the lines and polygons around the geographic regions illustrated on the `leaflet` map.
- `clean_data.R`: this function processes and cleans the data and returns a data frame that has been deduped and had any incorrect organisation/sampling location data removed.
- `clear_map_layers.R`: This function removes the layers of the map when the user changes the selected data type.
- `config.R`: This file contains the bulk of the text of the application and is intended to be the single place to quickly make text edits. The contents of this file are documented more extensively in the `man/config.Rd` file, but please use this file to edit the question text, drop down and radio button text, certain plot settings including palettes and bins.
- `create_and_pop_db.R`: This function loops through the different types of data and calls `clean_data.R`, then `db_create, populate_db` for each data type.
- `data_type_input.R`: A generalised control for the data type dropdown list that is reused across the tabs.
- `extra_taxa_input.R`: A control for the data input form that allows for adding custom taxa abundances.
- `information_card.R` and `newsletter_card.R`: Card functions that take the data out of the associated `.yml` file and put them into the UI.
- `make_<data type>_plot_data.R` functions: These functions process the data into the format needed to create `leaflet` and `ggplot` plots. 
- `populate_db.R`: This is a very small wrapper around `DBI` functions that puts the data into the SQLite database.
- `process_locations.R`: Creates location data as needed for `leaflet` from human-readable location tables.
- `small_helpers.R`: A collection of small helper files (<3 lines) that help process the data.
- `sum_up_ARMI.R`: Function to add up the ARMI scores across taxa.
- `turn_gsheets_into_db.R`: This is a function that runs all of the needed code to extract the data from the origin Google Sheet, clean it, and populate the database.

### Image and text content
Per `R` package standards, static content is included in the `app/www/` folder. Shapefile content for the `leaflet` map are within in `extdata` folder.

### Documentation
Documentation for all exported functions is included in the `man` folder in the appropriate `.Rd` file.

### Test files
- `R/test_helper.R`:
- `tests/testthat/` files:

### `golem` files
This package was developed using `golem` and includes several files containing the name `golem` that were not modified. For more information on `golem`, see its documentation [here](https://engineering-shiny.org/golem.html).

The `dev` folder contains three `R` scripts that document the process of creating the app from initialising it, developing the contents, and deploying it. 

### Database
- `data.sqlite`: SQLite database (created automatically when package is loaded or installed)

## Notes
- All data is stored locally in `data.sqlite`.