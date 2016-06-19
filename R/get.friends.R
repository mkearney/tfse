#' get_friends
#'
#' @param user Screen name or user id of target user.
#' @param token OAuth token (1.0 or 2.0)
#' @param page Default \code{page = -1} specifies first page of json results. Other pages specified via cursor values supplied by Twitter API response object.
#' @param stringify logical, indicating whether to return user ids as strings (some ids are too long to be read as numeric). Defaults to \code{TRUE}
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return friends User ids for everyone a user follows.
#' @export
get_friends <- function(user, token, page = "-1", stringify = TRUE) {
  #if (is_screen_name(user)) {
  #  id_type <- "screen_name"
  #} else {
    id_type <- "user_id"
  #}

  parameters <- paste0("cursor=",
                       page, "&",
                       id_type, "=", user)

  if (stringify) parameters <- paste0(parameters, "&stringify_ids=true")

  out <- TWIT(query = "friends/ids",
              parameters = parameters,
              token = token)

  out$id
}

#' get_friends_max
#'
#' @param ids Data frame with column name "screen_name"
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @param start Starting value (nth user id)
#' @param stringify logical, indicating whether to return user ids as strings (some ids are too long to be read as numeric). Defaults to \code{TRUE}.
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return friends List of user ids each user follows.
#' @export
get_friends_max <- function(ids, tokens, start = 1, stringify = TRUE) {
  user_ids <- sapply(ids, function(x) as.list(x))
  first <- start

  for(i in tokens) {
    remaining <- check_rate_limit(type = "friends", i)

    if (remaining > 0) {
      last <- first + remaining - 1

      if (last > length(ids)) {
        last <- length(ids)
      }

      if (!stringify) {
        o <- lapply(user_ids[first:last], function(x) get_friends(x, i, stringify = FALSE))
      }

      o <- lapply(user_ids[first:last], function(x) get_friends(x, i))

      o <- data.frame(id = unlist(user_ids[first:last]),
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
  }

  out
}


#' get_friendslist
#'
#' Returns a cursored collection of user objects for every user the specified user is following (otherwise known as their “friends”).
#' @param user Screen name or user id of target user.
#' @param token OAuth token (1.0 or 2.0)
#' @param page Default \code{page = -1} specifies first page of json results. Other pages specified via cursor values supplied by Twitter API response object.
#' @seealso See \url{https://api.twitter.com/1.1/friends/list.json}.
#' @return json user object (nested list)
#' @export
get_friendslist <- function(user, token, page = "-1") {
  if (is_screen_name(user)) {
    id_type <- "screen_name"
  } else {
    id_type <- "id"
  }

  out <- TWIT(query = "friends/list",
              parameters = paste0("count=200&cursor=",
                                  page, "&",
                                  id_type, "=", user,
                                  "&skip_status=true&include_user_entities=false"),
              token = token)

  out
}

#' get_friendships
#'
#' Returns detailed information about the relationship between two arbitrary users.
#' @param source Screen name or user id of source user.
#' @param target Screen name or user id of target user.
#' @param token OAuth token (1.0 or 2.0)
#' @seealso See \url{https://api.twitter.com/1.1/friendships/show.json}.
#' @return json object (nested list)
#' @export
get_friendships <- function(source, target, token) {
  if (is_screen_name(source)) {
    id_type <- "screen_name"
  } else {
    id_type <- "id"
  }

  out <- TWIT(query = "friendships/show",
              parameters = paste0("source_", id_type, "=", source,
                                  "target_", id_type, "=", target),
              token = token)

  out
}
