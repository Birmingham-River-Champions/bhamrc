# Birmingham River Champions Shiny App
[![coverage](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/nmfs-ost/ghactions4r/refs/heads/badges/coverage-badge.json)](https://github.com/Birmingham-River-Champions/bhamrc/tree/badges)

This is an R Shiny app with a lightweight SQLite database and a dynamic input form. Users can submit data, which is stored in the database and displayed in the app.

## Features
- Modern, light-themed UI
- Dynamic input form for name, email, and comment
- Stores submissions in a local SQLite database
- Displays submitted entries in a table

## Setup Instructions
1. Ensure you have R and the following packages installed:
   - shiny
   - shinythemes
   - DBI
   - RSQLite
2. Open `app.R` in RStudio or run with R:
   ```R
   shiny::runApp('app.R')
   ```
3. The app will create a `data.sqlite` file in the project directory for local data storage.

## Usage
- Fill out the form and click "Submit".
- View all submissions in the table on the right.

## File Structure
- `app.R`: Main Shiny app file
- `data.sqlite`: SQLite database (created automatically)

## Notes
- All data is stored locally in `data.sqlite`.
- UI uses the "flatly" theme for a modern look.
