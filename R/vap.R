#' @export
lply <- function(.x, .f) {
  lapply(.x, rlang::as_closure(.f))
}

#' @export
sply <- function(.x, .f, ...) {
  args <- list(...)
  if ("USE.NAMES" %in% args) {
    USE.NAMES <- args$USE.NAMES
  } else {
    USE.NAMES <- FALSE
  }
  sapply(.x, rlang::as_closure(.f), USE.NAMES = USE.NAMES)
}


#' @export
mply <- function(.f, ...) {
  args <- list(rlang::as_closure(.f), ...)
  if (!"USE.NAMES" %in% args) {
    args$USE.NAMES <- FALSE
  }
  do.call("Map", args)
}

#' @export
vply_lgl <- function(.x, .f) {
  vapply(.x, rlang::as_closure(.f), FUN.VALUE = logical(1), USE.NAMES = FALSE)
}

#' @export
vply_dbl <- function(.x, .f) {
  vapply(.x, rlang::as_closure(.f), FUN.VALUE = numeric(1), USE.NAMES = FALSE)
}

#' @export
vply_int <- function(.x, .f) {
  vapply(.x, rlang::as_closure(.f), FUN.VALUE = integer(1), USE.NAMES = FALSE)
}

#' @export
vply_fct <- function(.x, .f) {
  vapply(.x, rlang::as_closure(.f), FUN.VALUE = factor(1), USE.NAMES = FALSE)
}

#' @export
vply_chr <- function(.x, .f) {
  vapply(.x, rlang::as_closure(.f), FUN.VALUE = character(1), USE.NAMES = FALSE)
}
