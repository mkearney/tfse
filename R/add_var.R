
#' Mutates new variable, adding it to data frame
#'
#' Adds new variable column to data frame
#'
#' @export
add_var <- function(x, ...) {
  dots <- list(...)
  if (!is.null(names(dots))) {
    varname <- names(dots)
  } else {
    varname <- deparse(substitute(...))
  }
  x[[varname]] <- unlist(dots, use.names = FALSE)
  x
}

add_var_ <- function(x, name, value) {
  x[[name]] <- value
  x
}

#' Add one or more named variables to data frame
#'
#' Adds additional columns for every named variable provided
#'
#' @param x Data frame to be added to.
#' @param ... Named variables to add to data frame
#' @return A data frame.
#' @examples
#'
#' ## add to vars to mtcars data set
#' add_vars(mtcars[, 1:4], a = TRUE, b = rnorm(32))
#'
#' @export
#' @rdname add_var
add_vars <- function(x, ...) {
  dots <- list(...)
  vars <- names(dots)
  for (i in seq_along(dots)) {
    x <- add_var_(x, vars[i], dots[[i]])
  }
  x
}
