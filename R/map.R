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

#' @examples
#' map_lgl(is.numeric, list(rnorm(10), rnorm(10)))
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

map_dbl(mean, list(rnorm(10), rnorm(10)), trim = 1)
map_int(mean, list(rnorm(10), rnorm(10)), trim = 1)
map_chr(mean, list(rnorm(10), rnorm(10)), trim = 1)
