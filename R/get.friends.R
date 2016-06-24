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
#' @param user_ids Data frame with column name "screen_name"
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @param start Starting value (nth user id)
#' @param stringify logical, indicating whether to return user ids as strings
#'   (some ids are too long to be read as numeric). Defaults to \code{TRUE}.
#' @param path Path name to save rds data file.
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return friends List of user ids each user follows.
#' @import dplyr
#' @import readr
#' @export
get_friends_max <- function(user_ids, tokens, start = 1, stringify = TRUE, path = NULL) {
  # starting value
  n <- start

  # create list item
  l <- list()

  # max rate limit of tokens exceeds remaining # ids
  for (i in tokens) {
    if (!stringify) {
      l_token <- sapply(user_ids[which_ids(n)], function(x) get_friends(x, i, stringify = FALSE))
    } else {
      l_token <- sapply(user_ids[which_ids(n)], function(x) get_friends(x, i))
    }
    l <- c(l, l_token)

    if (n %% 10 == 0) {
      cat(paste0(n, " of ", start + length(tokens), "\n"))
    } else {
      cat("*")
    }

    n <- n + 1

    if (length(l) + (start * 15) > length(user_ids)) break
  }

  d <- data_frame(user_id = user_ids[1:length(l)], friends = l)

  d$date <- Sys.Date()
  d <- d[, c(1, 3, 2)]

  if (!is.null(path)) {
    write_rds(d, path)
  }

  d
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
