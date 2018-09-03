#' Paste collapse input and print as message
#'
#' @param ... Strings to be paste0 collapse = ""
#' @param print Logical indicating whether to print the message (default) or return an unevaluated expression
#' @return Either invisibly returns text of message or unevaluated expression
#' @export
pmsg <- function(..., print = TRUE) {
  pmsg_(list(...), print = print)
}


pmsg_ <- function(dots, print = TRUE) {
  msg <- paste(dots, collapse = "")
  if (print) {
    message(msg)
  }
  invisible(msg)
}
