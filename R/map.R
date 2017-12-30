#' map_
#'
#' maps function and returns list
#'
#' @param f Function
#' @param ... Other args passed to function, i.e., named parameters
#'   specific to f function, and also the \code{MoreArgs} argument
#'   from \code{mapply}.
#' @return List
#' @details Yes, this is identical to base \code{Map} but it also
#'   hardwires USE.NAMES to FALSE.
#' @examples
#' map_(as.data.frame, mtcars)
#' @rdname map_
#' @export
map_ <- function(f, ...) {
  f <- match.fun(f)
  mapply(FUN = f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

#' map_chr_
#'
#' maps function and returns character vector
#'
#' @inheritParams map_
#' @return Vector type character
#' @examples
#' x <- c("A", "brown", "car", "drove", "eighty", "four", "gallons", "how")
#' map_chr_(function(x) substr(x, 1, 1), x)
#' @rdname map_
#' @export
map_chr_ <- function(f, ...) {
  f <- match.fun(f)
  x <- mapply(FUN = f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (any(vapply(x, is.recursive, logical(1)))) {
    stop("Expected atomic but returned recursive.", call.. = FALSE)
  }
  if (any(lengths(x) > 1L)) {
    msg <- which(lengths(x) > 1L)
    msg <- paste0("")
    stop("Return object length exceeded 1.")
  }
  if (any(lengths(x) == 0L)) {
    x[lengths(x) == 0L] <- NA_character_
  }
  x <- unlist(x, use.names = FALSE)
  if (!inherits(x, "character")) {
    x <- as.character(x)
  }
  x
}




#' map_dbl_
#'
#' maps function and returns double vector
#'
#' @inheritParams map_
#' @return Vector type double
#' @examples
#' x <- rnorm(10)
#' map_dbl_(round, x, 2)
#' @rdname map_
#' @export
map_dbl_ <- function(f, ...) {
  f <- match.fun(f)
  x <- mapply(FUN = f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (any(vapply(x, is.recursive, logical(1)))) {
    stop("Expected atomic but returned recursive.", call.. = FALSE)
  }
  if (any(lengths(x) > 1L)) {
    stop("Return object length exceeded 1.")
  }
  if (any(lengths(x) == 0L)) {
    x[lengths(x) == 0L] <- NA_real_
  }
  x <- unlist(x, use.names = FALSE)
  if (!inherits(x, "numeric")) {
    x <- as.double(x)
  }
  x
}

#' map_int_
#'
#' maps function and returns integer vector
#'
#' @inheritParams map_
#' @return Vector type integer
#' @rdname map_
#' @examples
#' x <- rnorm(10, 100, 5)
#' map_int_(as.integer, x, 2)
#' @export
map_int_ <- function(f, ...) {
  f <- match.fun(f)
  x <- mapply(FUN = f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (any(vapply(x, is.recursive, logical(1)))) {
    stop("Expected atomic but returned recursive.", call.. = FALSE)
  }
  if (any(lengths(x) > 1L)) {
    stop("Return object length exceeded 1.")
  }
  if (any(lengths(x) == 0L)) {
    x[lengths(x) == 0L] <- NA_integer_
  }
  x <- unlist(x, use.names = FALSE)
  if (!inherits(x, "integer")) {
    x <- as.integer(x)
  }
  x
}

#' map_lgl
#'
#' maps function and returns logical vector
#'
#' @inheritParams map_
#' @return Vector type logical.
#' @rdname map_
#' @examples
#' map_lgl_(is.numeric, list(rnorm(10), rnorm(10)))
#' @export
map_lgl_ <- function(f, ...) {
  f <- match.fun(f)
  x <- mapply(FUN = f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (any(vapply(x, is.recursive, logical(1)))) {
    stop("Expected atomic but returned recursive.", call.. = FALSE)
  }
  if (any(lengths(x) > 1L)) {
    stop("Return object length exceeded 1.")
  }
  if (any(lengths(x) == 0L)) {
    x[lengths(x) == 0L] <- NA
  }
  x <- unlist(x, use.names = FALSE)
  if (!inherits(x, "logical")) {
    x <- as.logical(x)
  }
  x
}
