#' map_chr
#'
#' @param f Function
#' @param ... Other args passed to function.
#' @return Vector type character
#' @examples
#' x <- c("A", "brown", "car", "drove", "eighty", "four", "gallons", "how")
#' map_chr(function(x) substr(x, 1, 1), x)
#' @export 
map_chr <- function(f, ...) {
  f <- match.fun(f)
  x <- mapply(FUN = f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (any(vapply(x, is.recursive, logical(1)))) {
    stop("Expected atomic but returned recursive.", call.. = FALSE)
  }
  x <- unlist(x, use.names = FALSE)
  if (!inherits(x, "character")) {
    x <- as.character(x)
  }
  x
}

#' map_dbl
#'
#' @param f Function
#' @param ... Other args passed to function.
#' @return Vector type double
#' @examples
#' x <- rnorm(10)
#' map_dbl(round, x, 2)
#' @export 
map_dbl <- function(f, ...) {
  f <- match.fun(f)
  x <- mapply(FUN = f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (any(vapply(x, is.recursive, logical(1)))) {
    stop("Expected atomic but returned recursive.", call.. = FALSE)
  }
  x <- unlist(x, use.names = FALSE)
  if (!inherits(x, "numeric")) {
    x <- as.double(x)
  }
  x
}

#' map_int
#'
#' @param f Function
#' @param ... Other args passed to function.
#' @return Vector type integer
#' @export 
map_int <- function(f, ...) {
  f <- match.fun(f)
  x <- mapply(FUN = f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (any(vapply(x, is.recursive, logical(1)))) {
    stop("Expected atomic but returned recursive.", call.. = FALSE)
  }
  x <- unlist(x, use.names = FALSE)
  if (!inherits(x, "integer")) {
    x <- as.integer(x)
  }
  x
}

#' map_lgl
#'
#' @param f Function
#' @param ... Other args passed to function.
#' @return Vector type logical.
#' @examples
#' map_lgl(is.numeric, list(rnorm(10), rnorm(10)))
#' @export
map_lgl <- function(f, ...) {
  f <- match.fun(f)
  x <- mapply(FUN = f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (any(vapply(x, is.recursive, logical(1)))) {
    stop("Expected atomic but returned recursive.", call.. = FALSE)
  }
  x <- unlist(x, use.names = FALSE)
  if (!inherits(x, "logical")) {
    x <- as.logical(x)
  }
  x
}
