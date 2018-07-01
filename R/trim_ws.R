
#' trim_ws
#'
#' Returns character vector without extra spaces and trimmed of white space.
#'
#' @param x Character vector
#' @return Character vector without extra spaces
#' @export
trim_ws <- function(x) {
  x <- gsub("[ ]{2,}", " ", x)
  gsub("^[ ]+|[ ]+$", "", x)
}
