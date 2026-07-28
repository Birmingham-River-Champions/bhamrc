# R/be_startup.R
be_startup <- function() {
  # Start polling for completed jobs
  be_poll()

  # Trigger initial refresh immediately
  be_start()

  # Schedule future refreshes
  be_schedule_check(
    interval = 1
  )
}
