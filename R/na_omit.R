all_na <- function(x) all(is.na(unlist(x, use.names = FALSE)) | lengths(x) == 0)

#' Omit missing values/rows
#'
#' Returns data object with NA values (if atomic vector), all NA
#' elements (if list), or all NA rows (if data frame or matrix)
#' omitted
#'
#' @param x Data object
#' @return Data with NA values (if atomic vector), all NA elements (if
#'   list), or all NA rows (if data frame or matrix) omitted.
#' @examples
#'
#' ## generate data
#' df <- data.frame(
#'   a = I(list(c(1, 2), c(NA_integer_, NA_integer_), c(1, 2))),
#'   b = c("a", NA_character_, "c"),
#'   c = c(1.1243, NA_real_, -1.234134)
#' )
#'
#' ## data frame
#' na_omit(df)
#'
#' ## matrix
#' na_omit(as.matrix(df))
#'
#' ## list
#' na_omit(apply(df, 1, c))
#'
#' ## atomic vector
#' na_omit(df$b)
#'
#' @export
na_omit <- function(x) UseMethod("na_omit")

#' @export
na_omit.default <- function(x) x[!is.na(x)]

#' @export
na_omit.list <- function(x) {
  na_elems <- vapply(x, all_na, logical(1))
  x[!na_elems]
}

#' @export
na_omit.data.frame <- function(x) {
  na_rows <- vapply(
    seq_len(nrow(x)),
    function(i) all_na(x[i, ]), logical(1),
    USE.NAMES = FALSE
  )
  x[!na_rows, , drop = FALSE]
}

#' @export
na_omit.matrix <- function(x) {
  na_rows <- vapply(
    seq_len(nrow(x)),
    function(i) all_na(x[i, ]), logical(1),
    USE.NAMES = FALSE
  )
  x[!na_rows, , drop = FALSE]
}
