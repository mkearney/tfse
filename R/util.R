#' try_catch
#'
#' @param x function call
#' @export
try_catch <- function(x) {
  tryCatch(x, error = function(e) NA)
}

#' fromJS
#'
#' @param x json object
#' @import jsonlite
#' @export
fromJS <- function(x) {
  fromJSON(content(x, as = "text", encoding = "UTF-8"))
}

#' foo_params
#'
#' @param x stream search string
#' @export
track_encode <- function(x) {
  if (length(x) > 0) x <- paste(x, collapse = ",")
  paste(sapply(unlist(strsplit(x, split = ",")), function(x) URLencode(trimws(x), reserved = FALSE)), collapse = ",")
}

#' foo_params
#'
#' @param x api params
#' @export
foo_params <- function(x) {
  if (length(x) == 0) return(FALSE)
  if (nchar(x) > 20) return(TRUE)
  FALSE
}

#' get_api
#'
#' @param url API url address.
#' @return Response formatted as nested list.
#' Assumes response object is json object.
#' @import httr
#' @import jsonlite
#' @export
get_api <- function(url, token = NULL) {
  if (is.null(token)) {
    req <- GET(url)
  } else {
    req <- GET(url, config(token = as.character(token)))
  }

  if (http_error(req)) {
    return(NULL)
  }

  out <- fromJSON(content(req, as = "text"))
  out
}



