#' TWIT
#'
#' @param query Twitter API request type string. e.g, \code{"friends/ids"} calls
#'   Twitter API to return information about a user's friend network (i.e.,
#'   accounts followed by a user).
#' @param parameters Additional parameters passed along to API call
#' @param token An OAuth token (1.0 or 2.0)
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return json response object as nested list
#' @import httr
#' @import jsonlite
#' @export
TWIT <- function(query, parameters = NULL, token, parse = TRUE, version = "1.1") {
  # POST and GET requests
  if (query == "lists/members") {
    req <- POST(paste0("https://api.twitter.com/",
                       version, "/",
                       query,
                       ".json?",
                       parameters),
                config = config(token = token))
  } else {
    req <- GET(paste0("https://api.twitter.com/",
                      version, "/",
                      query,
                      ".json?",
                      parameters),
               config = config(token = token))
  }

  if (http_error(req)) {
    return(invisible())
  }
  if(parse) {
    return(fromJS(req))
  }
  req
}

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

#' get_token
#'
#' @param app Name of user created Twitter application
#' @param consumer_key Application API key
#' @param consumer_secret Application API secret User-owned app must have
#'   \code{Read and write} access level and \code{Callback URL} of
#'   \code{http://127.0.0.1:1410}.
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return twitter oauth 1.0 token
#' @import httr
#' @export
get_token <- function(app, consumer_key, consumer_secret) {
  out <- oauth_app(appname = app,
                   key = consumer_key,
                   secret = consumer_secret)
  out <- oauth1.0_token(oauth_endpoints("twitter"), out)
  out
}

#' is_screen_name
#'
#' @param x Twitter user id or screen name
#' @return logical value indicating whether object is screen name [or user ID]
#' @export
is_screen_name <- function(x) suppressWarnings(is.na(as.numeric(x)))

#' check_rate_limit
#'
#' @param type If not missing, which returns entire rate limit request object,
#'   type returns specific values; e.g., \code{type = "lookup"} returns
#'   remaining limit for user lookup requests; \code{type = "followers"} returns
#'   remaining limit for follower id requests; \code{type = "friends"} returns
#'   remaining limit for friend id requests.
#' @param token An OAuth token (1.0 or 2.0)
#' @param seconds logical, indicating whether to return unix seconds
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return response Rate limit response object or specific value of remaining
#'   requests
#' @export
check_rate_limit <- function(type, token, seconds = FALSE) {
  rate_limit_status <- TWIT("application/rate_limit_status",
                            parameters = NULL,
                            token = token)
  if (seconds) {
    response_type <- "reset"
  } else {
    response_type <- "remaining"
  }
  if (missing(token)) {
    return(rate_limit_status)
  }
  if ("lookup" %in% tolower(type)) {
    out <- rate_limit_status$resources$users$`/users/lookup`[[response_type]]
  }
  if ("followers" %in% tolower(type) | "follower" %in% tolower(type)) {
    out <- rate_limit_status$resources$followers$`/followers/ids`[[response_type]]
  }
  if ("friends" %in% tolower(type) | "friend" %in% tolower(type)) {
    out <- rate_limit_status$resources$friends$`/friends/ids`[[response_type]]
  }
  if (seconds) {
    out <- as.POSIXct(out, origin = "1970-01-01")
  }
  out
}

