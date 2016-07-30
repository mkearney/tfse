

#' .lookup_tweets
#'
#' @param users Screen name or user id of target users.
#' @param token OAuth tokens (1.0 or 2.0)
#' @param df logical, indicating whether to format response as data frame
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return response object
#' @export
.lookup_tweets <- function(tweet_ids, token = NULL,
                          df = TRUE, map = FALSE) {
  if (class(tweet_ids) == "list") {
    tweet_ids <- unlist(tweet_ids)
  }

  if (length(tweet_ids) > 100) {
    tweet_ids <- tweet_ids[1:100]
  }

  parameters <- paste0("id=", paste(tweet_ids,
                                    collapse = ","))

  if (is.null(token)) {
    token <- get.twitter.tokens(1)
    token <- token[[1]]
  }
  if (is.integer(token)) {
    token <- get.twitter.tokens(token)
    token <- token[[1]]
  }

  tw_df <- TWIT(query = "statuses/lookup",
                parameters = parameters,
                token = token)

  if (!parse) return(tw_df)

  parse_tweets(tw_df)
}


#' lookup_tweets
#'
#' @param ids User id or screen name of target user.
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @param start First (nth) id
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return response object
#' @export
lookup_tweets <- function(tweet_ids, tokens = NULL, start = 1) {
  first <- start
  total <- length(tweet_ids)

  if (is.null(tokens)) {
    tokens <- get.twitter.tokens()
  }

  tw_df <- dplyr::data_frame()

  for (i in seq_along(tokens)) {

    remaining <- check_rate_limit(type = "statuses",
                                  tokens[[i]])

    while (remaining > 0) {
      last <- first + 99

      if (last > total) {
        last <- total
      }

      tw_new <- lookup_tweets(tweet_ids[first:last],
                              tokens[[i]])

      if (!is.data.frame(tw_new)) break

      tw_df <- dplyr::bind_rows(tw_df, tw_new)

      first <- last + 1

      remaining <- remaining - 1

      if (!length(remainder) == 1) {
        break
      }
  }
  tw_df
}
