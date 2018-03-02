#' @export
lmap <- function(.x, .f) {
  lapply(.x, rlang::as_closure(.f))
}

#' @export
smap <- function(.x, .f, ...) {
  args <- list(...)
  if ("USE.NAMES" %in% args) {
    USE.NAMES <- args$USE.NAMES
  } else {
    USE.NAMES <- FALSE
  }
  sapply(.x, rlang::as_closure(.f), USE.NAMES = USE.NAMES)
}


#' @export
mmap <- function(.f, ...) {
  args <- list(rlang::as_closure(.f), ...)
  if (!"USE.NAMES" %in% args) {
    args$USE.NAMES <- FALSE
  }
  do.call("Map", args)
}

#' @export
vap_lgl <- function(.x, .f) {
  vapply(.x, rlang::as_closure(.f), FUN.VALUE = logical(1), USE.NAMES = FALSE)
}

#' @export
vap_dbl <- function(.x, .f) {
  vapply(.x, rlang::as_closure(.f), FUN.VALUE = numeric(1), USE.NAMES = FALSE)
}

#' @export
vap_int <- function(.x, .f) {
  vapply(.x, rlang::as_closure(.f), FUN.VALUE = integer(1), USE.NAMES = FALSE)
}

#' @export
vap_fct <- function(.x, .f) {
  vapply(.x, rlang::as_closure(.f), FUN.VALUE = factor(1), USE.NAMES = FALSE)
}

#' @export
vap_chr <- function(.x, .f) {
  vapply(.x, rlang::as_closure(.f), FUN.VALUE = character(1), USE.NAMES = FALSE)
}
