
#' do something with .x
#'
#' @param .x Data
#' @param expr Expression (the thing to do)
#' @return Evaluates expr with .x as environment
#' @export
with.x <- function(.x, expr) {
  rlang::eval_tidy(rlang::enquo(expr), data = .x)
}
