#' update_tweet_text
#'
#' Update text variable to include retweet/quote text as relevant
#'
#' @param x Data frame of tweets data.
#' @return Data frame with updated text column
#' @export
update_tweet_text <- function(x) {
  to_add <- ifelse(
    x$is_retweet, x$retweet_text,
    ifelse(x$is_quote, x$quoted_text, "")
  )
  x$text[x$is_retweet] <- x$retweet_text[x$is_retweet]
  x$text[x$is_quote] <- gsub(
    "https\\:\\/\\/t\\.co\\/[[:alnum:]]{10}$",
    "",
    x$text[x$is_quote]
  )
  x$text[x$is_quote] <- paste0(
    x$text[x$is_quote],
    x$quoted_text[x$is_quote]
  )
  x
}

