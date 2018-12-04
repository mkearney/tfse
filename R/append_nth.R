
#' Append integers with appopriate suffix
#'
#' Adds "st", "nd", "rd", or "th" suffix to integers
#'
#' @param x Integer or integerish vector.
#' @return A character vector with integers converted to "th"
#' @examples
#'
#' ## randomly sample ten values from 1:100 and add suffix
#' append_nth(sample(1:100, 10))
#'
#' @export
append_nth <- function(x) {
  append_nth_ <- function(x) {
    if (is.na(x)) return(NA_character_)
    if (grepl("1$", x)) {
      paste0(x, "st")
    } else if (grepl("2$", x)) {
      paste0(x, "nd")
    } else if (grepl("3$", x)) {
      paste0(x, "rd")
    } else {
      paste0(x, "th")
    }
  }
  if (is.numeric(x) && all((x %% 1) == 0)) {
    x <- as.integer(x)
  }
  stopifnot(is.integer(x))
  vapply(x, append_nth_, character(1))
}

