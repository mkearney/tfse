#' tweet url
#'
#' Returns URL to twitter status
#'
#' @param x Data with status ID and screen name variables.
#' @return URLs to statuses.
#' @export
tweet_url <- function(x) UseMethod("tweet_url")

#' @export
tweet_url.list <- function(x) {
  paste0("https://twitter.com/",
         paste(x[[1]], "status", x[[2]], sep = "/"))
}

#' @export
tweet_url.data.frame <- function(x) {
  x <- list(x$screen_name, x$status_id)
  tweet_url(x)
}


screenshot_tweet <- function(x, file) {
  if (!(is.character(x) && all(grepl("^http", x)))) {
    x <- tweet_url(x)
  }
  if (length(x) > 1L) {
    x <- x[1]
  }
  tryCatch(webshot::webshot(
    x, file,
    select = "#permalink-overlay-dialog > div.PermalinkOverlay-content > div > div > div.permalink.light-inline-actions.stream-uncapped.has-replies.original-permalink-page > div.permalink-inner.permalink-tweet-container > div",
#    selector = "div.permalink-inner.permalink-tweet-container",
    delay = .25,
    vheight = 2000
  ), error = function(e) return(NULL))
}
