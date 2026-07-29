#' Triggers check of backend job every second
#' @importFrom later later
#' @noRd
be_poll <- function() {
  be_check()

  later(
    be_poll,
    delay = 0.1
  )
}
