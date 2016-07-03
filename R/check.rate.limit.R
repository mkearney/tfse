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
