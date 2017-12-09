#' Determines whether vector can be converted to numeric
#'
#' Converts vector (if possible) into numeric and determines if it makes
#' sense to do so.
#'
#' @param x Vector to test whether it can be converted to numeric.
#' @return Logical value of true or false.
#' @export
can_num <- function(x) UseMethod("can_num")

#' @export
can_num.character <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  naprop <- sum(is.na(x)) / length(x)
  if (naprop < .5) return(TRUE)
  FALSE
}

#' @export
can_num.numeric <- function(x) TRUE

#' @export
can_num.integer <- function(x) TRUE

#' @export
can_num.list <- function(x) {
  if (any(lengths(x) > 1L)) return(FALSE)
  x <- as.character(x)
  NextMethod("can_num")
}

#' @export
can_num.data.frame <- function(x) FALSE

#' Determines whether vector can be converted to integer
#'
#' Converts vector (if possible) into integer and determines if it makes
#' sense to do so.
#'
#' @param x Vector to test whether it can be converted to integer.
#' @return Logical value of true or false.
#' @export
can_int <- function(x) UseMethod("can_int")

#' @export
can_int.character <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  naprop <- sum(is.na(x)) / length(x)
  if (naprop > .5) return(FALSE)
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    x <- na_omit(x)
    abs(x - round(x)) < tol
  }
  all(is.wholenumber(x))
}

#' @export
can_int.numeric <- function(x) FALSE

#' @export
can_int.integer <- function(x) TRUE

#' @export
can_int.list <- function(x) {
  if (any(lengths(x) > 1L)) return(FALSE)
  x <- as.character(x)
  NextMethod("can_int")
}

#' @export
can_int.data.frame <- function(x) FALSE
