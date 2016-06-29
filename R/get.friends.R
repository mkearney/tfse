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
get_friends <- function(user = user_ids, token, page = "-1", parse = TRUE, stringify = TRUE, timeout = 60) {
  parameters <- paste0("cursor=", page,
                       "&user_id=", user)
  if (stringify) parameters <- paste0(parameters, "&stringify_ids=true")

  if (parse) {
    out <- TWIT(query = "friends/ids",
                parameters = parameters,
                token = token,
                timeout = timeout)

    if (length(out) == 0) {
      return(NA_character_)
    }
    if (length(out$ids) == 0) {
      return(NA_character_)
    }
    return(out$ids)
  }

  out <- TWIT(query = "friends/ids",
              parameters = parameters,
              token = token,
              parse = FALSE,
              timeout = timeout)
  out
}

#' get_friends_max
#'
#' @param user_ids Data frame with column name "screen_name"
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @param start Starting value (nth user id)
#' @param stringify logical, indicating whether to return user ids as strings
#' @param verbose default behavior \code{verbose = TRUE} prints asterisk for every 15 (or max of one token) user networks collected. Set \code{verbose = FALSE} to run function silently.
#'   (some ids are too long to be read as numeric). Defaults to \code{TRUE}.
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return friends List of user ids each user follows.
#' @import dplyr
#' @export
get_friends_max <- function(user_ids, tokens, start = 1, stringify = TRUE, verbose = TRUE, timeout = 60, parse = TRUE) {
  # starting value
  n <- start

  # create list vector
  l <- vector("list", length(tokens))

  # max rate limit of tokens exceeds remaining # ids
  for (i in seq_along(tokens)) {
    if (!stringify) {
      l[[i]] <- sapply(user_ids[which_ids(n)], function(x)
        get_friends(x, tokens[[i]], parse = parse, timeout = timeout))
    } else {
      l[[i]] <- sapply(user_ids[which_ids(n)], function(x)
        get_friends(x, tokens[[i]], parse = parse, timeout = timeout))
    }

    if (verbose) {
      if (n %% 10 == 0) {
        cat(paste0(n, " of ", start + length(tokens), "\n"))
      } else {
        cat("*")
      }
    }

    n <- n + 1
  }

  l
}


#' fromJS
#'
#' @description parse json object
#' @import jsonlite
#' @import httr
#' @export
fromJS <- function(x) {
  if (is.null(x)) return(NA)
  out <- fromJSON(content(x, as = "text", encoding = "UTF-8"))
  if (length(out) == 0) return(NA)
  out
}

#' get_friends_ply
#'
#' @description lapply() version of get_friends_max()
#' @param user_ids Data frame with column name "screen_name"
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @param start Starting value (nth user id)
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return friends List of user ids each user follows.
#' @import dplyr
#' @export
get_friends_ply <- function(user_ids, tokens, start = 1) {
  # lapply
  l <- mapply(function(i, n) sapply(user_ids[which_ids(n)], function(x) get_friends(x, i)), tokens, start:length(tokens))

  # data frame
  d <- dplyr::data_frame(user_id = user_ids[1:length(l)], friends = lapply(l, function(x) list(x)[[1]]))
  d$date <- Sys.Date()
  d <- d[, c(1, 3, 2)]

  d
}

#' which_ids
#'
#' Returns integer values. Used for get_friends function.
#' @param n starting number for users
#' @param max_users max number of user ids (if rate limit exceeds remaining number of users, this sets upper ceiling and reduces likelihood of API request errors)
#' @param token Specify token if there is reason to believe current remaning friend list request is below the rate limit max of 15. This rate limit resets every 15 minutes, so this is usually not necessary. Checking rate limits does not reduce the number of available requests, but it does slow things down.
#' @return integers used to identify 15 (or token max given rate limits) users from provided list of user ids
#' @export
which_ids <- function(n, max_users = 3000, token = NULL) {
  if (!is.null(token)) {
    total <- check_rate_limit("friends", token)
    if (total == 0) {
      return(invisible())
    }
  } else {
    total <- 15
  }
  remain <- total - 1

  n <- n * (remain + 1) - remain
  end <- n + remain

  if (end > max_users) {
    end <- max_users
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
