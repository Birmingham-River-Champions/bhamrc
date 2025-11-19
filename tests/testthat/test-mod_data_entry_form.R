library(testthat)
library(shiny)

test_that("UI contains expected uiOutput id", {
    ui <- mod_data_entry_form_ui("myid")
    tags <- shiny::renderUI(ui)
    html <- tags$html
    expect_true(grepl("myid-form_ui", html))
})

test_that("server function exists", {
    expect_true(is.function(mod_data_entry_form_server))
})
