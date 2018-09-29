
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
  ds <- list.files(pattern = "\\.DS_Store$", recursive = TRUE, all.files = TRUE)
  unlink(ds)
  invisible()
}
