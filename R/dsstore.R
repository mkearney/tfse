
#' Remove pesky .DS_Store files
#'
#' Recursively removes all .DS_Store files in working directory.
#'
#' @examples
#'
#' \dontrun{
#' rm_.DS_Store()
#' }
#'
#' @export
rm_.DS_Store <- function() {
  sh <- system("find . -name '.DS_Store' -delete", intern = TRUE)
  invisible()
}
