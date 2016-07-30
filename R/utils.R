#' sn2id
#'
#' @param screen_name Twitter handle
#' @seealso See \url{https://dev.twitter.com/
#' overview/documentation} for more information on using
#' Twitter's API.
#' @return response Twitter account user id
#' @import rvest
sn2id <- function(screen_name) {
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("Rvest needed for this function to work.
         Please install it.",
         call. = FALSE)
  }

  user_id <- read_html(paste0("http://twitter.com/",
                              screen_name))

  user_id <- user_id %>% html_nodes(".ProfileNav") %>%
    html_attr("data-user-id")

  user_id
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
    req <- httr::GET(url, httr::config(token = token))
  }

  if (httr::http_error(req)) {
    return(NA)
  }

  from_js(req)
}
