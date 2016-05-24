#' get_lookup
#'
#' @param users User ids of target user.
#' @param token OAuth tokens (1.0 or 2.0)
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return response object
#' @export
get_lookup <- function(users, token) {

  if (length(users) > 100) {
    users <- users[1:100]
  }

  out <- TWIT(query = "users/lookup",
              parameters = paste0("user_id=",
                                  paste(users, collapse = ","),
                                  "&include_entities=false"
              ),
              token = token)

  return(out)
}

#' get_lookup_max
#'
#' @param ids User ids of target user.
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return response object
#' @export
get_lookup_max <- function(ids, tokens) {
  ids <- unique(ids)
  rate_limits <- sapply(tokens, function(x) check_rate_limit(type = "lookup", x))
  N <- sum(rate_limits * 100)
  ids <- sample(ids, N)
  tokens <- tokens[rate_limits > 0]
  hundos <- floor(length(ids)/100)
  remainder <- length(ids)/100 - floor(length(ids)/100)
  start_ids <- (0:hundos*100)+1
  end_ids <- 1:hundos*100
  end_ids <- c(end_ids, max(end_ids) + remainder*100)
  sets <- lapply(1:(hundos+1), function(x) start_ids[x]:end_ids[x])
  first <- 1

  colnames <- c("id", "screen_name", "location",
                "protected", "followers_count", "friends_count",
                "created_at","favourites_count","verified",
                "statuses_count", "lang")

  for(i in tokens) {
    remaining <- check_rate_limit(type = "lookup", i)
    last <- first + remaining - 1
    sets_sub <- sets[first:last]
    o <- sapply(sets_sub, function(x) get_lookup(ids[x], i))
    o <- lapply(o, function(x) {
      if (sum(c(colnames) %in% names(x)) == 11) {
        return(x[names(x) %in% colnames])
      }
    })

    o <- do.call(rbind, o)

    if (exists("out")) {
      out <- rbind(out, o)
    } else {
      out <- o
    }
    first <- last + 1
  }

  out$created_at <- as.Date(out$created_at, format = "%a %b %d %H:%M:%S %z %Y")
  out$tweets_per_day <- out$statuses_count / as.numeric(Sys.Date() - out$created_at + 1)
  out <- subset(out, protected == FALSE & followers_count > 100 & followers_count < 500 &
                  friends_count > 100 & friends_count < 500 & verified == FALSE &
                  statuses_count > 1000 & lang == "en" & tweets_per_day > .09)
  return(out)
}
