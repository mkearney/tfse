
#' Paste grapes
#'
#' Paste0 strings together with grapes (inverse/inside out function call)
#'
#' @param lhs Left hand side presumably character string
#' @param rhs Right hand side presumably another character string
#' @return A pasted together (with no space) string(s)
#' @export
'%P%' <- function(lhs, rhs) {
  UseMethod("%P%")
}

#' @export
'%P%.default' <- function(lhs, rhs) {
  if (is.recursive(lhs) || is.recursive(rhs)) {
    stop("`%P%` expected atomic but got recursive.", call. = FALSE)
  }
  paste0(lhs, rhs)
}
