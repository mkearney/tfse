#' TWIT
#'
#' @param type If not missing, which returns entire rate limit request object, type returns specific values; e.g., \code{type = "lookup"} returns remaining limit for user lookup requests; \code{type = "followers"} returns remaining limit for follower id requests; \code{type = "friends"} returns remaining limit for friend id requests.
#' @param query Twitter API request type string. e.g, \code{"friends/ids"} calls Twitter API to return information about a user's friend network (i.e., accounts followed by a user).
#' @param parameters Additional parameters passed along to API call
#' See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @param token An OAuth token (1.0 or 2.0)
#' @return json response object as nested list
#' @import httr
#' @import jsonlite
#' @export
TWIT <- function(query, parameters, token){
  if (is.null(parameters)){
    req <- GET(paste0("https://api.twitter.com/1.1/",
                      query,
                      ".json"),
               config(token = token))
  } else {
    req <- GET(paste0("https://api.twitter.com/1.1/",
                      query,
                      ".json?",
                      parameters),
               config(token = token))
  }
  if (http_error(req)){
    return(NULL)
  }
  out <- fromJSON(content(req, as = "text"))
  return(out)
}

#' load_tokens
#'
#' @return twitter oauth 1.0 tokens
#' @import httr
#' @export
load_tokens <- function(){
  source("/Users/mwk/r/tfse/twitter_tokens/create_twitter_tokens.R")
}

#' get_friends
#'
#' @param screen_name Screen name of target user.
#' @param token OAuth token (1.0 or 2.0)
#' @param page Default \code{page = -1} specifies first page of json results. Other pages specified via cursor values supplied by Twitter API response object.
#' See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return friends User ids for everyone a user follows.
#' @export
get_friends <- function(user, token, page = "-1"){
  if (is_screen_name(user)){
    id_type <- "user_id"
  } else{
    id_type <- "screen_name"
  }

  out <- TWIT(query = "friends/ids",
              parameters = paste0("count=5000&cursor=",
                                  page, "&",
                                  id_type, "=", user),
              token = token)

  return(out$id)
}

#' is_screen_name
#'
#' @param x Twitter user id or screen name
#' @return logical value indicating whether object is screen name [or user ID]
is_screen_name <- function(x) return(suppressWarnings(is.na(as.integer(x))))

#' get_friends_max
#'
#' @param followers Data frame with column name "screen_name"
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @param group Source used to identify users (groups are \code{%in% c("liberal", "conservative", "realDonaldTrump", "HillaryClinton", "celebrity")})
#' See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return friends List of user ids each user follows.
#' @export
get_friends_max <- function(ids, tokens, group, start){
  if (missing(start)){
    start <- 1
  }
  if (missing(group)){
    group <- NA
  }

  rate_limits <- sapply(tokens, function(x) check_rate_limit(type = "friends", x))
  N <- sum(rate_limits, na.rm = TRUE)

  if (N == 0) stop("I saw this wino, he was eating grapes. It's like, 'dude, you have to wait.' ~ Mitch Hedberg")

  ids <- ids[start:(start + N - 1)]
  tokens <- tokens[rate_limits > 0]
  user_ids <- sapply(ids, function(x) as.list(x))
  names(user_ids) <- as.character(ids)
  first <- 1

  for(i in tokens){
    remaining <- check_rate_limit(type = "friends", i)
    last <- first + remaining - 1
    user_sub <- user_ids[first:last]
    o <- lapply(user_sub, function(x) get_friends(x, i))
    #o <- o[! sapply(o, is.null) ]
    o <- data.frame(id = names(o),
                    group = group,
                    date = Sys.Date(),
                    friends = I(o),
                    row.names = NULL,
                    stringsAsFactors = FALSE)

    if (exists("out")){
      out <- rbind(out, o)
    } else{
      out <- o
    }
    first <- last + 1
  }

  return(out)
}

#' get_followers
#'
#' @param screen_name Screen name of target user.
#' @param token OAuth token (1.0 or 2.0)
#' See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @param page Default \code{page = -1} specifies first page of json results. Other pages specified via cursor values supplied by Twitter API response object.
#' @return user ids
#' @export
get_followers <- function(user, token, page = "-1"){
  if (is_screen_name(user)){
    id_type <- "user_id"
  } else{
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
#' See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return user ids
#' @export
get_followers_max <- function(user, tokens){
  rate_limits <- sapply(tokens, function(x) check_rate_limit(type = "followers", token = x))
  tokens <- tokens[rate_limits > 0]
  followers <- get_followers(user, tokens[[1]])
  page <- followers$next_cursor
  ids <- followers$ids

  for(i in tokens){
    followerid_limit <- check_rate_limit(type = "followers", token = i)

    while (followerid_limit > 0){
      followers <- get_followers(user, i, page)
      ids <- c(ids, followers$ids)
      page <- followers$next_cursor

      if ( page == 0){
        break
      }

      followerid_limit <- check_rate_limit(type = "followers", token = i)
    }
  }
  return(ids)
}

#' check_rate_limit
#'
#' @param type If not missing, which returns entire rate limit request object, type returns specific values; e.g., \code{type = "lookup"} returns remaining limit for user lookup requests; \code{type = "followers"} returns remaining limit for follower id requests; \code{type = "friends"} returns remaining limit for friend id requests.
#' @param token An OAuth token (1.0 or 2.0)
#' See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return response Rate limit response object or specific value of remaining requests
#' @export
check_rate_limit <- function(type, token){
  rate_limit_status <- TWIT("application/rate_limit_status", parameters = NULL, token = token)
  if ( missing(token)){
    return(rate_limit_status)
  }
  if ("lookup" %in% tolower(type)){
    out <- rate_limit_status$resources$users$`/users/lookup`$remaining
    return(out)
  }
  if ("followers" %in% tolower(type)){
    out <- rate_limit_status$resources$followers$`/followers/ids`$remaining
    return(out)
  }
  if ("friends" %in% tolower(type)){
    out <- rate_limit_status$resources$friends$`/friends/ids`$remaining
    return(out)
  }
}

#' get_lookup
#'
#' @param users User ids of target user.
#' @param tokens OAuth tokens (1.0 or 2.0)
#' See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return response object
#' @export
get_lookup <- function(users, token){

  if (length(users) > 100){
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
#' See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return response object
#' @export
get_lookup_max <- function(ids, tokens){
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

  for(i in tokens){
    remaining <- check_rate_limit(type = "lookup", i)
    last <- first + remaining - 1
    sets_sub <- sets[first:last]
    o <- sapply(sets_sub, function(x) get_lookup(ids[x], i))
    o <- lapply(o, function(x){
      if (sum(c(colnames) %in% names(x)) == 11){
        return(x[names(x) %in% colnames])
      }
      })

    o <- do.call(rbind, o)

    if (exists("out")){
      out <- rbind(out, o)
    } else{
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

#' get_friends_timepoint
#'
#' @param followers Data frame with user ids in column one
#' @param tokens OAuth tokens (1.0 or 2.0)
#' @parem screen_name Origin twitter account followed
#' @N minimum number of friends to return
#' See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return response object
#' @export
get_friends_timepoint <- function(followers, tokens, screen_name, N = 600){
  while (nrow(out) > N) {
    if (!exists("out")){
      out <- get_friends_max(followers$id, tokens, screen_name, 1)
    } else{
      new <- get_friends_max(followers$id, tokens, screen_name, nrow(out) + 1)
      out <- rbind(out, new)
    }
    if(nrow(out) > N){
      break
    } else {
      Sys.sleep(15*60)
    }
  }
}
