#' Set names
#'
#' Add names with a parenthetical function.
#'
#' @param x Data object
#' @param nms Names to assign object.
#' @export
set_names <- function(x, nms) {
  `names<-`(x, nms)
}

#' Unset row names
#'
#' Unset row names with a parenthetical function.
#'
#' @param x Data object
#' @export
unset_row_names <- function(x) {
  `row.names<-`(x, NULL)
}
