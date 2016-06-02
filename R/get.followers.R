
#' get_followers
#'
#' @param user Screen name or user id of target user.
#' @param token OAuth token (1.0 or 2.0)
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @param page Default \code{page = -1} specifies first page of json results. Other pages specified via cursor values supplied by Twitter API response object.
#' @return user ids
#' @export
get_followers <- function(user, token, page = "-1") {
  if (is_screen_name(user)) {
    id_type <- "user_id"
  } else {
    id_type <- "screen_name"
  }

  out <- TWIT(query = "followers/ids",
              parameters = paste0("count=5000&cursor=",
                                  page, "&",
                                  id_type, "=", user),
              token = token)
  return(out)
}

#' get_followers_max
#'
#' @param user Screen name or user id of target user
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return user ids
#' @export
get_followers_max <- function(user, tokens) {
  rate_limits <- sapply(tokens, function(x) check_rate_limit(type = "followers", token = x))
  tokens <- tokens[rate_limits > 0]
  followers <- get_followers(user, tokens[[1]])
  page <- followers$next_cursor
  ids <- followers$ids

  for(i in tokens) {
    followerid_limit <- check_rate_limit(type = "followers", token = i)

    while (followerid_limit > 0) {
      followers <- get_followers(user, i, page)
      ids <- c(ids, followers$ids)
      page <- followers$next_cursor

      if ( page == 0) {
        break
      }

      followerid_limit <- check_rate_limit(type = "followers", token = i)
    }
  }
  return(ids)
}


#' get_followerslist
#'
#' Returns a cursored collection of user objects for users following the specified user.
#' @param user Screen name or user id of target user.
#' @param token OAuth token (1.0 or 2.0)
#' @param page Default \code{page = -1} specifies first page of json results. Other pages specified via cursor values supplied by Twitter API response object.
#' @seealso See \url{https://api.twitter.com/1.1/followers/list.json}.
#' @return json user object (nested list)
#' @export
get_followerslist <- function(user, token, page = "-1") {
  if (is_screen_name(user)) {
    id_type <- "screen_name"
  } else {
    id_type <- "id"
  }

  out <- TWIT(query = "followers/list",
              parameters = paste0("count=200&cursor=",
                                  page, "&",
                                  id_type, "=", user,
                                  "&skip_status=true&include_user_entities=false"),
              token = token)

  return(out)
}
