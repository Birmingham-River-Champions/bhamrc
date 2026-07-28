# Creates environment to store back end variables
# Runs before shiny process starts
be <- new.env(parent = emptyenv())

be$running <- FALSE
be$job <- NULL
be$timestamp <- "Never"
be$data <- NULL
be$run <- 0

be$started <- FALSE
