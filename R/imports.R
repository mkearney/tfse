#' Pipe
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' tbl
#'
#' @name as_tbl
#' @rdname as_tbl
#' @keywords internal
#' @export
#' @importFrom tibble as_tibble
#' @usage as_tbl(data.frame(x = rnorm(10), y = rnorm(10)))
as_tbl <- function(...) {
  tibble::as_tibble(..., validate = FALSE)
}


#' tibble
#'
#' @name tbl
#' @rdname tbl
#' @keywords internal
#' @export
#' @importFrom tibble data_frame
#' @usage data_frame(data.frame(x = rnorm(10), y = rnorm(10)))
tbl <- function(...) {
  tibble::data_frame(...)
}

