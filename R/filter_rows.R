
#' Filter rows
#'
#' Filter rows via integer/numeric position or logical vector
#'
#' @param x Data frame or two dimensional array
#' @param ... This should evaluate and reduce down to a numeric (row number)
#'   or logical vector. Row numbers higher than what exists in x will be
#'   ignored. Any numeric vector must be either all positive or all negative.
#' @return Sliced/filtered data frame
#' @export
filter_rows <- function(x, ...) UseMethod("filter_rows")

#' @export
filter_rows.default <- function(x, ...) {
  if (length(dim(x)) != 2) {
    stop("filter_rows method requires two-dimensional object", call. = FALSE)
  }
  i <- unlist(lapply(list(...), eval))
  if (length(i) == 0) return(x)
  if (is.logical(i)) i <- which(i)
  i <- i[i <= nrow(x)]
  `[`(x, i, )
}
