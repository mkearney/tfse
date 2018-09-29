

#' @param x Input vector
#' @param na_omit Logical indicating whether to drop missing (NA) values.
#'   Default is TRUE.
#' @return Rescaled vector
#' @examples
#' ## randomly sample 10 values ranging from -10 to 100
#' x <- sample(-10:100, 10)
#'
#' ## rescale to 0-1 scale
#' rescale_standard(x)
#'
#' ## rescale to normal distribution (z-scores)
#' rescale_normal(x)
#'
#' ## rescale to logged distribution (natural log)
#' rescale_log(x)
#'
#' ## rescale to new point scale
#' rescale_pointscale(x, 1, 7, lower0 = -10, upper0 = 100)
#'
#' @name rescale
NULL

#' Standard: Rescale values to a standard normal scale
#'
#' @export
#' @rdname rescale
rescale_standard <- function(x, na_omit = TRUE) UseMethod("rescale_standard")

#' @export
rescale_standard.default <- function(x, na_omit = TRUE) {
  stopifnot(is.numeric(x))
  x <- add_class(x, "standard_scale")
  rescale_scale(x, na_omit = na_omit)
}

#' Normal: Rescale values to a standard (0-1) scale
#'
#' @inheritParams rescale
#' @export
#' @rdname rescale
rescale_normal <- function(x, na_omit = TRUE) UseMethod("rescale_normal")

#' @export
rescale_normal.default <- function(x, na_omit = TRUE) {
  stopifnot(is.numeric(x))
  if (length(x) == 1) return(x)
  x <- add_class(x, "normal_scale")
  rescale_scale(x, na_omit = na_omit)
}


#' Log: Rescale values to a natural log scale
#'
#' @inheritParams rescale
#' @export
#' @rdname rescale
rescale_log <- function(x, na_omit = TRUE) UseMethod("rescale_log")

#' @export
rescale_log.default <- function(x, na_omit = TRUE) {
  stopifnot(is.numeric(x))
  x <- add_class(x, "log_scale")
  rescale_scale(x, na_omit = na_omit)
}



#' Point-scale: Rescale values to a new point scale
#'
#' @inheritParams rescale
#' @param lower Min value of new scale.
#'   Only applicable for pointscales.
#' @param upper Max value of new scale.
#'   Only applicable for pointscales.
#' @param lower0 Min value of old scale. If NULL, defaults to min of input.
#'   Only applicable for pointscales.
#' @param upper0 Max value of old scale. If NULL, defaults to max of input
#'   Only applicable for pointscales.
#' @export
#' @rdname rescale
rescale_pointscale <- function(x, lower, upper,
                               lower0 = NULL, upper0 = NULL,
                               na_omit = TRUE) {
  UseMethod("rescale_pointscale")
}

#' @export
rescale_pointscale.default <- function(x, lower, upper,
                                       lower0 = NULL, upper0 = NULL,
                                       na_omit = TRUE) {
  stopifnot(is.numeric(x))
  x <- add_class(x, "pointscale_scale")
  rescale_scale(x, na_omit = na_omit, lower = lower, upper = upper,
    lower0 = lower0, upper0 = upper0)
}




rescale_scale <- function(x, na_omit = TRUE,
                          lower = 0, upper = 1,
                          lower0 = NULL, upper0 = NULL) {
  UseMethod("rescale_scale")
}

rescale_scale.standard_scale <- function(x, na_omit = TRUE) {
  x <- as.numeric(x)
  xmin <- min(x, na.rm = na_omit)
  (x - xmin) / (max(x, na.rm = na_omit) - xmin)
}

rescale_scale.normal_scale <- function(x, na_omit = TRUE) {
  if (na_omit)
    x <- na_omit(x)
  x <- as.numeric(x)
  scale(x)[, 1]
}

rescale_scale.log_scale <- function(x, na_omit = TRUE) {
  if (na_omit)
    x <- na_omit(x)
  x <- as.numeric(x)
  ## set min to at least 1
  if (min(x) < 1)
    x <- x - (min(x) - 1)
  log10(x)
}

rescale_scale.pointscale_scale <- function(x, lower = 0, upper = 1,
                                           lower0 = NULL, upper0 = NULL,
                                           na_omit = TRUE) {
  if (na_omit)
    x <- na_omit(x)
  ## set min to 0
  l <- min(c(lower0, x))
  x <- x - (l - 0)
  if (!is.null(upper0))
    upper0 <- upper0 - (l - 0)
  ## divide by max and multiply by new scale
  x / max(c(x, upper0)) * (upper - lower) + lower
}
