#' Data frame
#'
#' @name data_frame_
#' @rdname data.frame
#' @keywords internal
#' @export
data_frame_ <- function(...) {
	data.frame(..., stringsAsFactors = FALSE, row.names = NULL)
}

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
