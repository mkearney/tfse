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
#' @export
TWIT <- function(query, parameters = NULL, token,
                 parse = TRUE, version = "1.1",
                 timeout = 120, file_name) {
  # POST and GET requests
  if (query == "lists/members") {
    req <- httr::RETRY("POST",
                       paste0("https://api.twitter.com/",
                              version, "/",
                              query,
                              ".json?",
                              parameters),
                       config = httr::config(token = token))

  } else if (query == "statuses/filter") {

    if (should_be_post(parameters)) {
      httr::RETRY("POST",
        paste0(
          "https://stream.twitter.com/",
          version, "/",
          "statuses/filter.json?",
          parameters),
        config = httr::config(token = token),
        httr::timeout(timeout),
        httr::write_disk(file_name, overwrite = TRUE),
        times = 20)

      return(invisible())
    } else {
      httr::RETRY("GET",
        paste0("https://stream.twitter.com/",
               version, "/",
               "statuses/filter.json?",
               parameters),
        config = httr::config(token = token),
        httr::timeout(timeout),
        httr::write_disk(file_name, overwrite = TRUE),
        times = 20)

      return(invisible())
    }
  } else {
    req <- httr::RETRY("GET",
                       paste0("https://api.twitter.com/",
                              version, "/",
                              query,
                              ".json?",
                              parameters),
                       config = httr::config(token = token))
  }

  if (httr::http_error(req)) {
    return (message("http error"))
  }
  if (parse) {
    return (from_js(req))
  }
  req
}
