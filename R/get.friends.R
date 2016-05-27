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
    id_type <- "screen_name"
  } else {
    id_type <- "user_id"
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
get_friends_max <- function(ids, tokens, start = 1) {
  user_ids <- sapply(ids, function(x) as.list(x))
  first <- start

  for(i in tokens) {
    remaining <- check_rate_limit(type = "friends", i)
    last <- first + remaining - 1

    if (last > length(ids)) {
      last <- length(ids)
    }

    o <- lapply(user_ids[first:last], function(x) get_friends(x, i))

    o <- data.frame(id = names(o),
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

    if (first > length(ids)) break
  }

  return(out)
}



