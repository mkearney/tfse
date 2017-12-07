

#' Build url for HTTP request
#'
#' Builds HTTP request for a given API call.
#'
#' @param url Base URL should include base, path, version, etc.
#' @param ... Named arguments beyond url are treated as named
#'   query parameters to be passed along in the API call.
#' @return Character string with API request URL.
#' @examples
#'
#' ## request URL
#' rurl <- api_call(
#'   "https://api.twitter.com/1.1/search/tweets.json",
#'   q = "rstats",
#'   count = 100
#' )
#'
#' \dontrun{
#' ## send http request (this should return error without access token)
#' r <- httr::GET(rurl)
#'
#' ## view returned content
#' httr::content(r)
#' }
#'
#' @export
api_call <- function(url, ...) {
  stopifnot(grepl("^http", url))
  dots <- list(...)
  if (length(dots) == 1L && is.list(dots[[1]])) {
    dots <- dots[[1]]
  }
  if (length(dots) > 0L) {
    dots <- paste0(names(dots), "=", dots)
    dots <- paste(dots, collapse = "&")
    url <- paste0(url, "?", dots)
  }
  url
}
