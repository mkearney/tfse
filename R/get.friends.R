#' get_friends
#'
#' @param screen_name Screen name of target user.
#' @param token OAuth token (1.0 or 2.0)
#' @param page Default \code{page = -1} specifies first page of json results. Other pages specified via cursor values supplied by Twitter API response object.
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return friends User ids for everyone a user follows.
#' @export
get_friends <- function(user, token, page = "-1") {
  if (is_screen_name(user)) {
    id_type <- "user_id"
  } else {
    id_type <- "screen_name"
  }

  out <- TWIT(query = "friends/ids",
              parameters = paste0("count=5000&cursor=",
                                  page, "&",
                                  id_type, "=", user),
              token = token)

  return(out$id)
}

#' get_friends_max
#'
#' @param ids Data frame with column name "screen_name"
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @param group Source used to identify users (groups are liberal, conservative, realDonaldTrump, HillaryClinton, and celebrity)
#' @param start Starting value (nth user id)
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return friends List of user ids each user follows.
#' @export
get_friends_max <- function(ids, tokens, group, start) {
  if (missing(start)) {
    start <- 1
  }
  if (missing(group)) {
    group <- NA
  }

  rate_limits <- sapply(tokens, function(x) check_rate_limit(type = "friends", x))
  N <- sum(rate_limits, na.rm = TRUE)

  if (N == 0) stop("I saw this wino, he was eating grapes. It's like, 'dude, you have to wait.' ~ Mitch Hedberg")

  ids <- ids[start:(start + N - 1)]
  tokens <- tokens[rate_limits > 0]
  user_ids <- sapply(ids, function(x) as.list(x))
  names(user_ids) <- as.character(ids)
  first <- 1

  for(i in tokens) {
    remaining <- check_rate_limit(type = "friends", i)
    last <- first + remaining - 1
    user_sub <- user_ids[first:last]
    o <- lapply(user_sub, function(x) get_friends(x, i))
    #o <- o[! sapply(o, is.null) ]
    o <- data.frame(id = names(o),
                    group = group,
                    date = Sys.Date(),
                    friends = I(o),
                    row.names = NULL,
                    stringsAsFactors = FALSE)

    if (exists("out")) {
      out <- rbind(out, o)
    } else {
      out <- o
    }
    first <- last + 1
  }

  return(out)
}


#' get_friends_timepoint
#'
#' @param followers Data frame with user ids in column one
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @param screen_name Origin twitter account followed
#' @param N minimum number of friends to return
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return response object
#' @export
get_friends_timepoint <- function(followers, tokens, screen_name, N = 600) {
  while (nrow(out) > N) {
    if (!exists("out")) {
      out <- get_friends_max(followers$id, tokens, screen_name, 1)
    } else {
      new <- get_friends_max(followers$id, tokens, screen_name, nrow(out) + 1)
      out <- rbind(out, new)
    }
    if (nrow(out) > N) {
      break
    } else {
      Sys.sleep(15 * 60)
    }
  }
}
