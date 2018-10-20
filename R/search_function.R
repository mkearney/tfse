
#' search functions
#'
#' Like search_files but for functions
#'
#' @param pat Pattern to match
#' @param fun Function to inspect
#' @export
search_function <- function(pat, fun) {
  f <- paste(as.character(as.list(fun)), collapse = "\n")
  dir <- tempdir()
  path <- file.path(dir, "fun")
  writeLines(f, path)
  search_files(pat, dir)
}
