#' execute expression quietly
#'
#' @param expr Expression to be evaluated without additional printing.
#' @return Output from evaluated expression.
#' @export
shhh <- function(expr) {
  utils::capture.output(x <- suppressPackageStartupMessages(
    suppressMessages(suppressWarnings(expr))))
  invisible(x)
}
