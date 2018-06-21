
#' tabsort
#'
#' Returns a sorted (descending) frequence tbl
#'
#' @param x Character vector
#' @param V1 Optional, name of term variable. Defaults to "term".
#' @param percent Logical indicating whether to include a percent of total
#'   column.
#' @param na_omit Logical indicating whether to exclude missing. If all
#'   responses are missing, a missing value is used as the single category.
#' @return Frequency tbl
#' @export
tabsort <- function(x, V1 = NULL, percent = TRUE, na_omit = TRUE) {
  if (is.atomic(x) && all(is.na(x))) {
    x <- table(x, useNA = "ifany")
  } else {
    x <- sort(table(x), decreasing = TRUE)
  }
  x <- data.frame(
    term = names(x), n = as.integer(x), stringsAsFactors = FALSE)
  if (percent) {
    x$percent <- x$n / sum(x$n, na.rm = TRUE)
  }
  if (!is.null(V1)) {
    names(x)[1] <- V1
  }
  x
}
