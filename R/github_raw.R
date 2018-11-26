
owner_repo <- function(x) {
  x <- sub("https?://github.com/?", "", x)
  regmatches_first(x, "[^/]+/[^/]+")
}


#' Generate link to raw Github file
#'
#' Converts Github path/repo/file information into a link to the raw version of
#' the file
#'
#' @param file Name of desired file; if \code{repo} is NULL, then this value
#'   should also provide repo information, i.e., owner and name of the
#'   repository–e.g., \code{"owner/repo/file.ext"}. Alternatively, user may
#'   supply the URL to the file as it appears in a web browser, e.g.,
#'   \code{"https://github.com/mkearney/driven-snow/blob/master/theme/driven-snow.rstheme"}
#' @param repo Repository name information formatted as username/repo. If this
#'   information is provided in the value supplied to \code{file} then leave
#'   this as NULL (the default)
#' @return Returns the URL path to the raw version of the file.
#' @export
github_raw <- function(file, repo = NULL) {

  ## remove scheme and domain
  file <- sub("https?://github.com/?", "", file)

  ## if repo not provided
  if (is.null(repo)) {
    repo <- owner_repo(file)
    file <- sub("^[^/]+/[^/]+/", "", file)
  }

  ## if 'blob'
  if (grepl("^blob", file)) {
    file <- sub("blob/master/", "", file)
  }

  ## if 'tree' path
  if (grepl("^tree", file)) {
    stop("this is a path to a Github directory not a file", call. = FALSE)
  }

  ## validate strings
  stopifnot(
    length(file) == 1,
    length(repo) == 1,
    grepl("[^/]+/[^/]+", repo)
  )

  ## return raw version of file
  sprintf("https://raw.githubusercontent.com/%s/master/%s",
    repo, file)
}


paste_lines <- function(...) paste(c(...), collapse = "\n")


check_all_gh_files <- function(file, repo) {
  ## get all possible files
  all_files <- readlines(
    psub("https://github.com/{repo}?_pjax=%23js-repo-pjax-container",
      repo = repo)
  )
  all_files <- regmatches_(paste_lines(all_files), "(?<=href=\")[^\"]+")[[1]]
  all_files <- unique(
    grep(psub("^/{repo}/\\S+", repo = repo), all_files, value = TRUE)
  )
  all_files <- grep("blob/master", all_files, value = TRUE)
  all_files <- sub(psub("^/{repo}/blob/master/", repo = repo), "", all_files)

  ## if the file exists
  stopifnot(file %in% all_files)
}
