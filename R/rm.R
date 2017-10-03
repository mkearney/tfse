#' rm_amp
#' 
#' Remove ampersands as they show up in tweet data
#' 
#' @param x Input character vector
#' @param y Replacement text
#' @return Output character vector
#' @export
rm_amp <- function(x, y = "&") {
  if (is.null(y)) y <- ""
  gsub("&amp;", y, x)
}


#' rm_linebreaks
#' 
#' Remove linebreaks
#' 
#' @param x Input character vector
#' @param y Replacement text
#' @return Output character vector
#' @export
rm_linebreaks <- function(x, y = " ") {
  gsub("\\n", y, x)
}

#' rm_retweets
#' 
#' Remove retweets from data.
#' 
#' @param x Input, should be either data frame (or list) or character.
#'   If list/data frame, the text of the tweet is assumed to be named
#'   "text" while the logical is_retweet is assuemd to be named "is_retweet"
#' @return Output
#' @export
rm_retweets <- function(x, ...) {
  UseMethod("rm_retweets")
}


#' @export
rm_retweets.data.frame <- function(x,
                                   text = "text",
                                   is_retweet = "is_retweet") {
  stopifnot(is.character(x[[text]]))
  if (is_retweet %in% names(x)) {
    stopifnot(is.logical(x[[is_retweet]]))
    is_retweet <- x[[is_retweet]]
  } else {
    is_retweet <- grepl(
      "RT \\@", x[[text]]
    )
  }
  x[[text]][is_retweet] <- ""
  x
}

#' @export
rm_retweets.list <- function(x,
                             text = "text",
                             is_retweet = "is_retweet") {
  stopifnot(is.character(x[[text]]))
  if (is_retweet %in% names(x)) {
    stopifnot(is.logical(x[[is_retweet]]))
    is_retweet <- x[[is_retweet]]
  } else {
    is_retweet <- grepl(
      "RT \\@", x[[text]]
    )
  }
  x[[text]][is_retweet] <- ""
  x
}

#' @export
rm_retweets.character <- function(x, is_retweet = NULL) {
  if (is.null(is_retweet)) {
    is_retweet <- grepl("RT \\@", x)
  }
  stopifnot(identical(length(x), length(is_retweet)))
  x[is_retweet] <- ""
  x
}
