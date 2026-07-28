# Checks if back end job running.
# Once complete moves data to be$data an sets flags
#' @param interval interval between checks
#' @importFrom later later
#' @noRd
be_schedule_check <- function(interval = be_interval) {
  be_start()

  later(
    function() {
      be_schedule_check(interval)
    },
    delay = interval
  )
}
