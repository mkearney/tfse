
#' Select columns with minimum amount of variance
#'
#' Filters numeric columns by requiring a minimum amount of variance
#'
#' @param x Input data, which should be either a data frame or matrix.
#' @param min Minimum amount of variance to require per column.
#' @return Returns data frame (or matrix, depending on input class) with all non-numeric
#'   columns and only those numeric columns that meet the minimum amount of variance.
#' @details This function omits missing values.
#' @examples
#'
#' ## set seed (for replication purposes)
#' set.seed(206195)
#'
#' ## create data set
#' d <- data_set(
#'   w = rnorm(100, 0, 0.0),
#'   v = rnorm(100, 0, 0.5),
#'   x = rnorm(100, 0, 1.0),
#'   y = rnorm(100, 0, 2.0),
#'   z = rnorm(100, 0, 3.0)
#' )
#'
#' ## minimum var of 1.0 (default)
#' min_var(d)
#'
#' ## min variance of 0.1
#' min_var(d, 0.1)
#'
#' ## min var of 2.0
#' min_var(d, 2.0)
#'
#' ## min var of 6.0
#' min_var(d, 6.0)
#'
#' @export
min_var <- function(x, min = 1) UseMethod("min_var")

#' @export
min_var.default <- function(x, min = 1) {
  stop(sprintf("Expected data frame or matrix input but got %s", class(x)[1]),
    call. = FALSE)
}

#' @export
min_var.list <- function(x, min = 1) {
  lapply(x, min_var, min = min)
}

#' @export
min_var.matrix <- function(x, min = 1) {
  stopifnot(is.numeric(x))
  x[, apply(x, 2, stats::var) >= min]
}

#' @export
min_var.data.frame <- function(x, min = 1) {
  is_num <- dapr::vap_lgl(x, is.numeric)
  non_num <- names(x)[!is_num]
  yminvar <- names(x[is_num])[
    vapply(x[is_num],
    function(.x) stats::var(.x, na.rm = TRUE) >= min,
    logical(1))
  ]
  x[c(non_num, yminvar)]
}
