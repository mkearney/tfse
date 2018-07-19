

time_zones <- c("UTC", "America/Chicago", "America/New_York",
  "America/Denver", "America/Los_Angeles")



#' Normalize scale(s)
#'
#' Transform numeric objects into z-scores
#'
#' @param x Data
#' @return Numeric objects transformed via standard normal distribution.
#' @export
scale_normal <- function(x) {
  scale(x)[, 1]
}


#' Standardize scale(s)
#'
#' Transform numeric objects into values on 0-1 scale
#'
#' @param x Data
#' @return Numeric objects transformed onto 0-1 scale
#' @export
scale_standard <- function(x) {
  xmin <- min(x, na.rm = TRUE)
  (x - xmin) / (max(x, na.rm = TRUE) - xmin)
}
