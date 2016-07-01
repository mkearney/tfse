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
