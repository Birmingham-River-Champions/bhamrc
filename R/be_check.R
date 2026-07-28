# Checks if backend job running.
# Once complete moves data to be$data an sets flags
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

  # pass result to be$data for convenience
  be$timestamp <- be$job$timestamp # Record when run

  be$run <- be$run + 1 #iterate run count

  # Update data value when backend has data
  if (!is.null(be$job$data)) {
    be$data <- be$job$data
  }

  # Clear job data and set job running flag to false
  be$job <- NULL
  be$running <- FALSE
}
