#' Data frame
#'
#' @name data_frame_
#' @rdname data.frame
#' @keywords internal
#' @export
data_frame_ <- function(...) {
	data.frame(..., stringsAsFactors = FALSE, row.names = NULL)
}

#' rbind_
#'
#' @name rbind_
#' @rdname rbind_
#' @keywords internal
#' @export
rbind_ <- function(...) {
  rbind(..., stringsAsFactors = FALSE)
}

#' dc_rbind_
#'
#' @name dc_rbind_
#' @rdname dc_rbind_
#' @keywords internal
#' @export
dc_rbind_ <- function(...) {
  do.call("rbind_", ...)
}

#' cbind_
#'
#' @name cbind_
#' @rdname cbind_
#' @keywords internal
#' @export
cbind_ <- function(...) {
  cbind(..., stringsAsFactors = FALSE)
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
