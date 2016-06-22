#' get_friends
#'
#' @param user Screen name or user id of target user.
#' @param token OAuth token (1.0 or 2.0)
#' @param page Default \code{page = -1} specifies first page of json results.
#'   Other pages specified via cursor values supplied by Twitter API response
#'   object.
#' @param stringify logical, indicating whether to return user ids as strings
#'   (some ids are too long to be read as numeric). Defaults to \code{TRUE}
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return friends User ids for everyone a user follows.
#' @export
get_friends <- function(user, token, page = "-1", stringify = TRUE) {
  parameters <- paste0("cursor=", page,
                       "&user_id=", user)

  if (stringify) parameters <- paste0(parameters, "&stringify_ids=true")

  out <- TWIT(query = "friends/ids",
              parameters = parameters,
              token = token)

  if (length(out) == 0) {
    return(NA_character_)
  }
  if (length(out$ids) == 0) {
    return(NA_character_)
  }

  list(out$ids)
}

#' get_friends_max
#'
#' @param ids Data frame with column name "screen_name"
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @param start Starting value (nth user id)
#' @param stringify logical, indicating whether to return user ids as strings
#'   (some ids are too long to be read as numeric). Defaults to \code{TRUE}.
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return friends List of user ids each user follows.
#' @import dplyr
#' @export
get_friends_max <- function(user_ids, tokens, n = 1, stringify = TRUE, verbose = TRUE) {
  start <- n
  for (i in tokens) {
    fdf_token <- get_friends_data(user_ids[which_ids(n)], i)
    #Sys.sleep(.1)

    if (exists("fdf")) {
      fdf <- c(fdf, fdf_token)
    } else {
      fdf <- fdf_token
    }
    if (verbose) cat("*")

    if (n > 200) {
      break
    } else {
      n <- n + 1
    }
  }
  data_frame(user_id = user_ids[start:length(fdf)], friends = fdf)
}

#' which_ids
#'
#' Returns integer values. Used for get_friends function.
#' @param n starting number for users
#' @return integers used to identify 15 users
#' @export
which_ids <- function(n) {
  remain <- 15 - 1

  n <- n * (remain + 1) - remain
  end <- n + remain

  if (end > 3000) {
    end <- 3000
  }
  n:end
}


#' get_friendslist
#'
#' Returns a cursored collection of user objects for every user the specified
#' user is following (otherwise known as their “friends”).
#' @param user Screen name or user id of target user.
#' @param token OAuth token (1.0 or 2.0)
#' @param page Default \code{page = -1} specifies first page of json results.
#'   Other pages specified via cursor values supplied by Twitter API response
#'   object.
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
#' Returns detailed information about the relationship between two arbitrary
#' users.
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
