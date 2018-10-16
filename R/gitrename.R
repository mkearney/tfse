
#' Rename git repo code
#'
#' Command line (bash) syntax for renaming a git repo
#'
#' @param new_url Optional, URL pointing to correct (renamed) repo. If NULL
#'   (default), "new_url" is printed in brackets.
#' @return text of git command to rename git repo
#' @export
rename_git_repo <- function(new_url = NULL) {
  if (is.null(new_url)) new_url <- "{new_url}"
  psub("git remote set-url origin {new_url}", new_url = new_url)
}
