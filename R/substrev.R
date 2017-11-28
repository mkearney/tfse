
#' Sub string reverse
#'
#' Returns portion of string starting from end of string (otherwise just like substr)
#'
#' @param x Character vector
#' @param start Number of characters to include relative to the last
#'   character position.
#' @param stop Specify the number of characters from the final
#'   character to set as the last character position.
#' @return Sub string with last i characters.
#' @export
substrev <- function(x, start, stop = 0) {
  substr(x, nchar(x) - start, nchar(x) - stop)
}
