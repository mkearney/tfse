
#' search_files
#'
#' Returns matching files and line numbers of given string pattern.
#'
#' @param x Pattern.
#' @param path Path on which to restrict search. Defaults to current working
#'   directory.
#' @param recursive logical
#' @param invert logical
#' @param file_names only show matching file names
#' @param all.files default false excludes dot files
#' @param intern logical
#' @return Output from terminal - file name, line number, and preview of
#'   matching text
#' @export
search_files <- function(x, path = ".", recursive = TRUE, invert = FALSE,
                         file_names = FALSE, all.files = FALSE, intern = FALSE) {
  args <- "-n"
  if (recursive) {
    args <- paste0(args, "r")
  }
  if (invert) {
    args <- paste0(args, "v")
  }
  if (file_names) {
    args <- paste0(args, "l")
  }
  if (!all.files) {
    dot_dirs <- grep("/\\.[^\\.]", list.dirs(recursive = recursive), value = TRUE)
    dot_dirs <- unique(regmatches(dot_dirs, regexpr("(?<=/)\\.[^/]+", dot_dirs, perl = TRUE)))
    dot_dirs <- paste0("{", paste(dot_dirs, collapse = ","), "}")
    args <- paste0(args, " --exclude-dir=", dot_dirs)
  }
  cmd <- paste0("grep ", args, " ", shQuote(x), " ", path, " -s")
  system(cmd, intern = intern)
}
