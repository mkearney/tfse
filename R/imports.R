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

#' tbl
#'
#' @name tbl_df
#' @rdname tbl
#' @keywords internal
#' @export
#' @importFrom dplyr tbl_df
#' @usage tbl_df(...)
NULL
