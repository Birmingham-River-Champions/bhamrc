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
