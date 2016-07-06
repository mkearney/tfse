#' TWIT
#'
#' The Twitter Search API is part of Twitter’s REST API.
#' It allows queries against the indices of recent or
#' popular Tweets and behaves similarly to, but not
#' exactly like the Search feature available in Twitter
#' mobile or web clients, such as Twitter.com search.
#' The Twitter Search API searches against a sampling of
#' recent Tweets published in the past 7 days. The Search
#' API is focused on relevance and not completeness.
#' This means that some Tweets and users may be missing
#' from search results. If you want to match for
#' completeness you should consider using a Streaming
#' API instead.
#'
#' @param query Twitter API request type (e.g., \code{"friends/ids"},
#' \code{"users/lookup"}, \code{"statuses/filter"}).
#' @param parameters Additional parameters passed along to API call
#' @param token An OAuth token (1.0 or 2.0)
#' @param parse logical indicating whether to parse json response object
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return json response object as nested list
#' @details httr jsonlite
#' @import httr
#' @export
TWIT <- function(query, parameters = NULL, token,
                 parse = TRUE, version = "1.1",
                 timeout = 120, file_name) {

  if (query == "lists/members") {
    req <- POST(paste0("https://api.twitter.com/",
                       version, "/",
                       query, ".json?",
                       parameters),
                config(token = token))

  } else if (query == "statuses/filter") {

    if (should_be_post(parameters)) {
      req <- POST(paste0("https://stream.twitter.com/",
                        version, "/",
                        query, ".json?",
                        parameters),
                 config(token = token),
                 timeout(timeout),
                 write_disk(file_name,
                            overwrite = TRUE))

      return(invisible())
    } else {
      req <- GET(paste0("https://stream.twitter.com/",
                        version, "/",
                        "statuses/filter.json?",
                        parameters),
                 config(token = token),
                 timeout(timeout),
                 write_disk(file_name,
                            overwrite = TRUE))

      return(invisible())
    }
  } else if (query == "friends/ids") {
    req <- GET(paste0("https://api.twitter.com/",
                        version, "/",
                        query, ".json?",
                        parameters),
               config(token = token))

    if (http_error(req)) {
      return(list(ids = NA))

    } else if (parse) {
      return(from_js(req))
    }
  } else {
    req <- GET(paste0("https://api.twitter.com/",
                      version, "/",
                      query, ".json?",
                      parameters),
               config(token = token))

    if (http_error(req)) {
      return(invisible())

    } else if (parse) {
      return(from_js(req))
    }
  }
  req
}
