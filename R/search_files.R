
#' search_files
#'
#' Returns matching files and line numbers of given string pattern.
#'
#' @param x Pattern.
#' @param path Path on which to restrict search. Defaults to current working
#'   directory.
#' @return Output from terminal - file name, line number, and preview of
#'   matching text
#' @export
search_files <- function(x, path = ".") {
  cmd <- paste0("grep -nr ", shQuote(x), " ", path, " -s")
  system(cmd, intern = TRUE)
}
