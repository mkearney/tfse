
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
