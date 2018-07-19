#' edit names
#'
#' Edit names, reassign them, and then return object
#'
#' @param x Object with names to be edited
#' @param expr Expression to perform on names. IMPORTANT: assumes names are .x
#' @return Returns renamed x object
#' @export
edit_names <- function(x, expr) {
  .x <- names(x)
  e <- new.env()
  assign(".x", .x, envir = e)
  names(x) <- rlang::with_env(e, expr)
  x
}

#' find namespaces from fully qualified references in a directory
#'
#' @param dir Name of directory to search, defaults to current dir.
#' @return Unique list of namespaces
#' @export
fully_qualified_namespaces <- function(dir = ".") {
  x <- search_files("::", dir, intern = TRUE)
  m <- gregexpr("[[:alnum:]\\.]+::", x)
  x <- unlist(regmatches(x, m))
  unique(sub("::$", "", x))
}
