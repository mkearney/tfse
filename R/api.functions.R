#' TWIT
#'
#' @param type If not missing, which returns entire rate limit request object, type returns specific values; e.g., \code{type = "lookup"} returns remaining limit for user lookup requests; \code{type = "followers"} returns remaining limit for follower id requests; \code{type = "friends"} returns remaining limit for friend id requests.
#' @param query Twitter API request type string. e.g, \code{"friends/ids"} calls Twitter API to return information about a user's friend network (i.e., accounts followed by a user).
#' @param parameters Additional parameters passed along to API call
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @param token An OAuth token (1.0 or 2.0)
#' @return json response object as nested list
#' @import httr
#' @import jsonlite
#' @export
TWIT <- function(query, parameters, token) {
  if (is.null(parameters)) {
    req <- GET(paste0("https://api.twitter.com/1.1/",
                      query,
                      ".json"),
               config(token = token))
  } else {
    req <- GET(paste0("https://api.twitter.com/1.1/",
                      query,
                      ".json?",
                      parameters),
               config(token = token))
  }
  if (http_error(req)) {
    return(NULL)
  }
  out <- fromJSON(content(req, as = "text"))
  return(out)
}

#' load_tokens
#'
#' @return twitter oauth 1.0 tokens
#' @import httr
#' @export
load_tokens <- function() {
  source("/Users/mwk/r/tfse/create.twitter.tokens.R")
}

#' is_screen_name
#'
#' @param x Twitter user id or screen name
#' @return logical value indicating whether object is screen name [or user ID]
is_screen_name <- function(x) return(suppressWarnings(is.na(as.numeric(x))))

#' check_rate_limit
#'
#' @param type If not missing, which returns entire rate limit request object, type returns specific values; e.g., \code{type = "lookup"} returns remaining limit for user lookup requests; \code{type = "followers"} returns remaining limit for follower id requests; \code{type = "friends"} returns remaining limit for friend id requests.
#' @param token An OAuth token (1.0 or 2.0)
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return response Rate limit response object or specific value of remaining requests
#' @export
check_rate_limit <- function(type, token) {
  rate_limit_status <- TWIT("application/rate_limit_status", parameters = NULL, token = token)
  if ( missing(token)) {
    return(rate_limit_status)
  }
  if ("lookup" %in% tolower(type)) {
    out <- rate_limit_status$resources$users$`/users/lookup`$remaining
    return(out)
  }
  if ("followers" %in% tolower(type)) {
    out <- rate_limit_status$resources$followers$`/followers/ids`$remaining
    return(out)
  }
  if ("friends" %in% tolower(type)) {
    out <- rate_limit_status$resources$friends$`/friends/ids`$remaining
    return(out)
  }
}


