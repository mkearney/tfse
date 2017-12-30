
#' Round date and time values
#'
#' A generic function for rounding date and time values
#'
#' @param x A vector of class POSIX or Date.
#' @param n Unit to round to. Defaults to mins. Numeric values treated
#'   as seconds. Otherwise this should be one of "mins", "hours", "days",
#'   "weeks", "months", "years" (plural optional).
#' @return If POSIXct then POSIX. If date then Date.
#' @export
#' @importFrom hms hms
#' @examples
#'
#' ## class posixct
#' round_time(Sys.time(), "12 hours")
#'
#' ## class date
#' unique(round_time(seq(Sys.Date(), Sys.Date() + 100, "1 day"), "weeks"))
round_time <- function(x, n) UseMethod("round_time")

#' @export
round_time.POSIXt <- function(x, n = "mins") {
  n <- parse_to_secs(n)
  as.POSIXct(hms::hms(as.numeric(x) %/% n * n))
}

#' @export
round_time.Date <- function(x, n = "months") {
  x <- as.POSIXct(x)
  as.Date(round_time(x, n))
}
