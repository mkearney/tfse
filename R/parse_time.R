
#' parse_to_secs
#' 
#' Verbs object
#' 
#' @param x Unit of time in secs, mins, hours, days, weeks, etc.
#' @return Number of seconds
#' @export
parse_to_secs <- function(x) {
  if (is.numeric(x)) {
    n <- x
  } else if (grepl("year", x)) {
    n <- 60 * 60 * 24 * 365
  } else if (grepl("month", x)) {
    n <- 60 * 60 * 24 * 30
  } else if (grepl("week", x)) {
    n <- 60 * 60 * 24 * 7
  } else if (grepl("day", x)) {
    n <- 60 * 60 * 24
  } else if (grepl("hour", x)) {
    n <- 60 * 60
  } else if (grepl("min", x)) {
    n <- 60
  } else if (grepl("sec", x)) {
    n <- 1
  } else {
    stop("must express time interval in secs, mins, hours, days, weeks, months, or years",
         call. = FALSE)
  }
  x <- as.double(gsub("[^[:digit:]|\\.]", "", x))
  if (any(is.na(x), identical(x, ""))) {
    x <- 1
  }
  n * x
}
