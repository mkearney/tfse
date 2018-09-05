

#' Filter based on proportion of missing data
#'
#' Returns columns that have less than or equal to a specified amount of missingness.
#'
#' @param x Input data frame or matrix.
#' @param max Maximum proportion of missingnesses allowed. Columns with higher
#'   proportions of missingness compared to this value will be dropped. Columns
#'   will only be returned if they have 1 - max proportion non-missing. This
#'   value must be between 0-1. It defaults to .05.
#' @return Data frame or matrix with columns with less than or equal to the
#'   max allowed proportion of missingness.
#' @export
max_na <- function(x, max = .05) UseMethod("max_na")

#' @export
max_na.default <- function(x, max = .05) {
  if (!is.data.frame(x) | is.matrix(x)) {
    stop("Must supply data frame or matrix")
  }
  x
}

#' @export
max_na.data.frame <- function(x, max = .05) {
  stopifnot(length(max) == 1, is.numeric(max), max >= 0, max <= 1)
  kp <- vapply(x, is_max_na, max, FUN.VALUE = logical(1), USE.NAMES = FALSE)
  x[kp]
}

#' @export
max_na.matrix <- function(x, max = .05) {
  stopifnot(length(max) == 1, is.numeric(max), max >= 0, max <= 1)
  kp <- vapply(x, is_max_na, max, FUN.VALUE = logical(1), USE.NAMES = FALSE)
  x[, kp]
}


is_max_na <- function(x, max = .05) {
  if (is.list(x)) {
    x <- vapply(x, function(.x) all(is.na(unlist(.x))), logical(1))
    p <- sum(x) / length(x)
    p <= max
  } else {
    stopifnot(is.atomic(x), length(x) > 0)
    p <- sum(is.na(x)) / length(x)
    p <= max
  }
}


#' Count missing values
#'
#' Returns counts of missing values
#'
#' @param x Input data.
#' @return Counts of missing observations
#' @export
count_na <- function(x) UseMethod("count_na")

#' @export
count_na.default <- function(x) {
  stopifnot(is.atomic(x), length(x) > 0)
  sum(is.na(x))
}

#' @export
count_na.list <- function(x) {
  x <- ifelse(lengths(x) > 1L, FALSE, vapply(x, function(.x) is.na(.x[1]), logical(1)))
  sum(x)
}

#' @export
count_na.data.frame <- function(x) {
  vapply(x, count_na, integer(1))
}



