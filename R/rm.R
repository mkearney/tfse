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
rm_retweets <- function(x) {
  UseMethod("rm_retweets")
}


#' @export
rm_retweets.data.frame <- function(x) {
  text <- "text"
  is_retweet <- "is_retweet"
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
rm_retweets.list <- function(x) {
  text <- "text"
  is_retweet <- "is_retweet"
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


#' rm_links
#'
#' Removes URL links included in tweets.
#'
#' @param x Character vector of Twitter statuses.
#' @return Character vector of statuses without URLs.
#' @export
rm_links <- function(x) {
  x <- gsub("\\s{0,1}http\\S{1,}\\s{0,1}", "", x)
  gsub("\\s{0,1}\\S{1,}\\.com\\b\\s{0,1}", "", x)
}

#' rm_stopwords
#'
#' Returns statuses with stop words removed
#'
#' @param x Vector of text.
#' @param stopwords Optional, stop words to be removed from text. Defaults to
#'   SMART stop words provided by tidytext package.
#' @return Character vector with stopwords removed
#' @export
rm_stopwords <- function(x, stopwords) {
  wordbreakor <- function(x) {
    x <- paste(x, collapse = "\\s{0,1}\\b|\\b\\s{0,1}")
    paste0("\\b", x, "\\b")
  }
  stopwords <- wordbreakor(stopwords)
  x <- gsub(stopwords, " ", x, perl = TRUE, ignore.case = TRUE)
  trim_ws(x)
}
