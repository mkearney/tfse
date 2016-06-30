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
  user_df <- data_frame(user_id = prep_vector(as.character(x$id_str)),
                        name = prep_vector(as.character(x$name)),
                        screen_name = prep_vector(as.character(x$screen_name)),
                        location = prep_vector(as.character(x$location)),
                        description = prep_vector(as.character(x$description)),
                        url = prep_vector(as.character(x$url)),
                        protected = prep_vector(as.logical(x$protected)),
                        followers_count = prep_vector(as.integer(x$followers_count)),
                        friends_count = prep_vector(as.integer(x$friends_count)),
                        listed_count = prep_vector(as.integer(x$listed_count)),
                        created_at = as.Date(as.POSIXct(prep_vector(as.character(x$created_at)), format="%a %b %d %H:%M:%S %z %Y"), format = "%Y-%M-%D"),
                        favourites_count = prep_vector(as.integer(x$favourites_count)),
                        utc_offset = prep_vector(as.integer(x$utc_offset)),
                        geo_enabled = prep_vector(as.logical(x$geo_enabled)),
                        verified = prep_vector(as.logical(x$verified)),
                        statuses_count = prep_vector(as.integer(x$statuses_count)),
                        lang = prep_vector(as.character(x$lang)))

  user_df
}

#' prep_vector
#'
#' @param x data to be vectorized
#' @export
prep_vector <- function(x) {
  if (length(x) == 0) return(NA)
  x[unlist(lapply(x, function(x) length(x) > 1))] <- unlist(lapply(x[unlist(lapply(x, function(x) length(x) > 1))], unlist))
  x[x == ""] <- NA
  x[x == NaN] <- NA
  x[length(x) == 0] <- NA
  x[is.null(x)] <- NA
  as.vector(x)
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
  ooo <- data_frame()

  for(i in tokens) {
    remaining <- check_rate_limit(type = "lookup", i)
    while (remaining > 0) {
      last <- first + 99

      if (last > total) {
        last <- total
      }

      oo <- get_lookup(ids[first:last], i)
      ooo <- bind_rows(ooo, oo)

      first <- last + 1

      if (first > total) {
        return(ooo)
      }

      remainder <- check_rate_limit(type = "lookup", i)

      if (!length(remainder) == 1) {
        break
      } else {
        remaining <- remainder
      }

    }
  }
  ooo
}
