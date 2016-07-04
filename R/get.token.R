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
