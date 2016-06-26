#' get_lookup
#'
#' @param users Screen name or user id of target users.
#' @param token OAuth tokens (1.0 or 2.0)
#' @param df logical, indicating whether to format response as data frame
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return response object
#' @export
get_lookup <- function(users, token, df = TRUE, skip = TRUE, entities = FALSE) {
  if (class(users) == "list") {
    users <- unlist(users)
  }

  if (length(users) > 100) {
    users <- users[1:100]
  }

  if (sum(sapply(users[1:3], is_screen_name)) == 0) {
    id_type <- "user_id"
  } else {
    id_type <- "screen_name"
  }

  parameters = paste0(id_type, "=",
                      paste(users, collapse = ","))

  if (skip) {
    parameters <- paste0(parameters, "&skip_status=true")
  }
  if (!entities) {
    parameters <- paste0(parameters, "&include_user_entities=false")
  }

  out <- TWIT(query = "users/lookup",
              parameters = parameters,
              token = token)

  if (df) {
    out <- data_frame_lookup(out)
  }

  out
}

#' data_frame_lookup
#'
#' @param x json resposne object from user lookup Twitter API call.
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return data frame
#' @import dplyr
#' @export
data_frame_lookup <- function(x) {
  x <- tbl_df(x)

  x <- x[, names(x)[names(x) %in% c("id_str", "screen_name", "protected",
                                    "followers_count", "friends_count",
                                    "created_at", "favourites_count",
                                    "verified", "statuses_count", "lang")] ]

  if (length(x) == 0) {
    return(invisible())
  }

  names(x)[names(x) == "id_str"] <- "user_id"
  if(length(x$created_at) > 0) {
    x$created_at <- as.Date(as.POSIXct(x$created_at, format="%a %b %d %H:%M:%S %z %Y"), format = "%Y-%M-%D")
  }
  x
}

#' get_lookup_max
#'
#' @param ids User id or screen name of target user.
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @param start First (nth) id
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more
#'   information on using Twitter's API.
#' @return response object
#' @export
get_lookup_max <- function(ids, tokens, start = 1) {
  first <- start
  total <- length(ids)
  out <- data_frame()

  for(i in tokens) {
    remaining <- check_rate_limit(type = "lookup", i)
    while (remaining > 0) {
      last <- first + 99

      if (last > total) {
        last <- total
      }

      o <- get_lookup(ids[first:last], i)
      out <- bind_rows(out, o)
      first <- last + 1

      if (first > total) {
        return(out)
      }

      remainder <- check_rate_limit(type = "lookup", i)

      if (!length(remainder) == 1) {
        break
      } else {
        remaining <- remainder
      }

    }
  }
  out
}
