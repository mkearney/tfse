
#' peel lists
#'
#' @param x Input data
#' @return Peeled object
#' @export
peel_lists <- function(x) {
  while (is.list(x) && length(x) == 1 && is.recursive(x[[1]])) {
    x <- x[[1]]
  }
  x
}
