#' sn2id
#'
#' @param screen_name Twitter handle
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return response Twitter account user id
#' @details rvest
#' @export
sn2id <- function(screen_name) {
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("Rvest needed for this function to work. Please install it.",
         call. = FALSE)
  }

  user_id <- read_html(paste0("http://twitter.com/", screen_name))

  user_id <- user_id %>% html_nodes(".ProfileNav") %>%
    html_attr("data-user-id")

  user_id
}

#' try_catch
#'
#' @param x function call
#' @export
try_catch <- function(x) {
  tryCatch(x, error = function(e) NA)
}

#' from_js
#'
#' @param x json object
#' @details jsonlite httr
#' @export
from_js <- function(x) {
  jsonlite::fromJSON(httr::content(x, as = "text", encoding = "UTF-8"))
}

#' enc_track_query
#'
#' @param .track stream search string
#' @export
enc_track_query <- function(.track) {
  if (length(.track) > 0) .track <- paste(.track, collapse = ",")
  paste(sapply(unlist(strsplit(.track, split = ",")), function(x)
    URLencode(trimws(x), reserved = FALSE)), collapse = ",")
}

#' should_be_post
#'
#' @param x api params
#' @return logical indicating whether the query exceeds the specified
#' cutoff point
#' @export
should_be_post <- function(.query, .nchar = 20) {
  if (length(.query) == 0) return(FALSE)
  if (nchar(.query) > .nchar) return(TRUE)
  return(FALSE)
}

#' get_api
#'
#' @param url API url address.
#' @return Response formatted as nested list.
#' Assumes response object is json object.
#' @details httr jsonlite
#' @export
get_api <- function(url, token = NULL) {
  if (is.null(token)) {
    req <- httr::GET(url)
  } else {
    req <- httr::GET(url, httr::config(token = as.character(token)))
  }

  if (httr::http_error(req)) {
    return(NULL)
  }

  out <- jsonlite::fromJSON(httr::content(req, as = "text"))
  out
}
