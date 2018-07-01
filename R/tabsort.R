
#' tabsort
#'
#' Returns a sorted (descending) frequence tbl
#'
#' @param data Data
#' @param prop Logical indicating whether to include a proportion of total
#'   obs column.
#' @param na_omit Logical indicating whether to exclude missing. If all
#'   responses are missing, a missing value is used as the single category.
#' @return Frequency tbl
#' @export
tabsort <- function(data, ..., prop = TRUE, na_omit = TRUE, sort = TRUE) {
  if (!is.recursive(data)) {
    data <- list(term = data)
  } else {
    vars <- tidyselect::vars_select(names(data), ...)
    if (length(vars) > 0) {
      data <- data[vars]
    }
  }
  if (na_omit) {
    data <- na_omit(data)
  }
  x <- as_tbl(do.call("table", data))
  if (prop) {
    x$prop <- x$n / sum(x$n, na.rm = TRUE)
  }
  if (sort) {
    x <- dplyr::arrange_all(x)
  }
  x
}
