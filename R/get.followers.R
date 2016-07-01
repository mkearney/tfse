#' get_followers
#'
#' @param user Screen name or user id of target user.
#' @param token OAuth token (1.0 or 2.0)
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @param page Default \code{page = -1} specifies first page of json results.
#'   Other pages specified via cursor values supplied by Twitter API response
#'   object.
#' @param stringify logical, indicating whether to return user ids as strings
#'   (some ids are too long to be read as numeric). Defaults to \code{TRUE}
#' @return user ids
#' @export
get_followers <- function(user, token, page = "-1", stringify = TRUE) {

  parameters <- paste0("count=5000&cursor=", page,
                       "&screen_name=", user)

  if (stringify) parameters <- paste0(parameters, "&stringify_ids=true")

  out <- try_catch(TWIT(query = "followers/ids",
                        parameters = paste0("cursor=", page,
                                            "&screen_name=", user,
                                            "&count=5000"),
                        token = token))

  if (length(out) == 0) {
    return(NULL)
  }
  out
}

#' get_followers_max
#'
#' @param user Screen name or user id of target user
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return user ids
#' @export
get_followers_max <- function(user, tokens) {
  page <- "-1"
  f <- character()

  for(i in tokens) {
    followerid_limit <- check_rate_limit(type = "followers", token = i)

    while (followerid_limit > 0) {
      flwrs <- get_followers(user, i, page)
      n <- length(flwrs$ids) - 1

      f[(length(f) + 1):(length(f) + n)] <- flwrs$ids

      page <- as.character(flwrs$next_cursor)

      if (!length(page) == 1) {
        return(f)
      }

      if (page == "0") {
        return(f)
      }

      followerid_limit <- check_rate_limit(type = "followers", token = i)
    }
  }
  f
}


#' get_followerslist
#'
#' Returns a cursored collection of user objects for users following the
#' specified user.
#' @param user Screen name or user id of target user.
#' @param token OAuth token (1.0 or 2.0)
#' @param page Default \code{page = -1} specifies first page of json results.
#'   Other pages specified via cursor values supplied by Twitter API response
#'   object.
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

  out
}
