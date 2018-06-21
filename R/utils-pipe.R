#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' tibble
#'
#' See \code{tibble::\link[tibble]{as_tibble}} for details.
#'
#' @name tibble
#' @rdname tibble
#' @keywords internal
#' @export
#' @importFrom tibble as_tibble
as_tbl <- tibble::as_tibble

#' tibble
#'
#' See \code{tibble::\link[tibble]{as_tibble}} for details.
#'
#' @name tibble
#' @rdname tibble
#' @keywords internal
#' @export
#' @importFrom tibble data_frame
tbl_df <- tibble::data_frame
