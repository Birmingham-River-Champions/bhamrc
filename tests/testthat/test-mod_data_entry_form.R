library(testthat)
library(shiny)

test_that("server function exists", {
    expect_true(is.function(mod_data_entry_form_server))
})

test_that("UI function exists", {
    expect_true(is.function(mod_data_entry_form_ui))
})

test_that("reactive values are correct", {
    ui_output <- mod_data_entry_form_ui("test_id")
})
