
#' Return lhs values not in rhs values
#'
#' @param lhs Values to check whether they are/not contained in the other
#' @param rhs Values to use as the reference
#' @param value Logical indicating whether to return the value or a logical vector
#' @examples
#'
#' ## a, b, zz in alphabet letters
#' yin(c("a", "b", "zz"), letters)
#'
#' ## a, b, zz NOT in alphabet letters
#' nin(c("a", "b", "zz"), letters)
#'
#' @export
nin <- function(lhs, rhs, value = TRUE) {
  x <- !lhs %in% rhs
  if (value) {
    x <- lhs[x]
  }
  x
}

#' @inheritParams nin
#' @rdname nin
#' @export
yin <- function(lhs, rhs, value = TRUE) {
  x <- lhs %in% rhs
  if (value) {
    x <- lhs[x]
  }
  x
}
