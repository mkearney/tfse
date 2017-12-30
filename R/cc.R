

#' Combine comma separated strings
#'
#' Split strings by comma into character vector(s)
#'
#' @param x Vector of comma separated character strings
#' @param simplify Logical indicating whether to return a character vector
#'   (the default) when the length of x is one. This argument does nothing
#'   if the length of x is greater than 1.
#' @return If length of x is 1 then a character vector otherwise a list of
#'   character vectors.
#' @examples
#'
#' ## comma separated alphabet
#' abcs <- paste(letters, collapse = ",")
#'
#' ## split single string
#' cc(abcs)
#'
#' ## return as list
#' cc(abcs, simplify = FALSE)
#'
#' ## select columns
#' mtcars[, cc("cyl,mpg,wt,gear")]
#'
#' ## character vector with multiple strings
#' x <- c("v1,v2,v3", "y1,y2,y5")
#'
#' ## convert strings into list of [split] character vectors
#' cc(x)
#'
#' @export
cc <- function(x, simplify) UseMethod("cc")

#' @export
cc.default <- function(x, simplify = TRUE) {
  if (simplify && length(x) == 1L) {
    cc_(x)
  } else {
    Map("cc_", x, USE.NAMES = FALSE)
  }
}

cc_ <- function(x) {
  strsplit(x, ",")[[1]]
}
