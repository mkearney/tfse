#' data_frame_status
#'
#' @param x json object from search tweets
#' @import dplyr
#' @export
data_frame_status <- function(x) {
  status_df <- data_frame(
    "status_id" = as.character(prep_vector(x$id_str)),
    "text" = as.character(prep_vector(x$text)),
    "truncated" = as.character(prep_vector(x$text)),
    "result_type" = as.character(prep_vector(x$metadata$result_type)),
    "created_at" = as.Date(as.POSIXct(as.character(prep_vector(x$created_at)),
                                      format="%a %b %d %H:%M:%S %z %Y"),
                           format = "%Y-%M-%D"),
    "source" = as.character(prep_vector(x$source)),
    "in_reply_to_status_id" = as.character(prep_vector(x$in_reply_to_status_id_str)),
    "in_reply_to_user_id" = as.character(prep_vector(x$in_reply_to_user_id_str)),
    "in_reply_to_screen_name" = as.character(prep_vector(x$in_reply_to_screen_name)),
    "is_quote_status" = as.character(prep_vector(x$is_quote_status)),
    "retweet_count" = as.character(prep_vector(x$retweet_count)),
    "favorite_count" = as.character(prep_vector(x$favorite_count)),
    "favorited" = as.character(prep_vector(x$favorited)),
    "retweeted" = as.character(prep_vector(x$retweeted)),
    "lang" = as.character(prep_vector(x$lang)),
    "quoted_status_id" = as.character(prep_vector(x$quoted_status_id_str)))

  if (length(x$place) > 0) {
    status_df <- bind_rows(status_df, parse_place(x$place))
  }
  if (length(x$entities) > 0) {
    status_df <- bind_rows(status_df, parse_status_entities(x$entities))
  }
  if (length(x$user) > 0) {
    status_df <- bind_rows(status_df, parse_user(x$user))
  }
  status_df
}


#' parse_place
#'
#' @param x json resposne object from user lookup Twitter API call.
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return data frame
#' @import dplyr
#' @export
parse_place <- function(x) {
  place_df <- data_frame(
    "place_id" = try_catch(prep_vector(x$id)),
    "place_url" = prep_vector(x$url),
    "place_type" = prep_vector(x$place_type),
    "place_name" = prep_vector(x$name),
    "place_full_name" = prep_vector(x$full_name),
    "place_country_code" = prep_vector(x$country_code),
    "place_country" = prep_vector(x$country),
    "place_long1" = lapply(x$bounding_box$coordinates, function(x) prep_vector(x[1, 1, 1])),
    "place_long2" = lapply(x$bounding_box$coordinates, function(x) prep_vector(x[1, 2, 1])),
    "place_long3" = lapply(x$bounding_box$coordinates, function(x) prep_vector(x[1, 3, 1])),
    "place_long4" = lapply(x$bounding_box$coordinates, function(x) prep_vector(x[1, 4, 1])),
    "place_lat1" = lapply(x$bounding_box$coordinates, function(x) prep_vector(x[1, 1, 2])),
    "place_lat2" = lapply(x$bounding_box$coordinates, function(x) prep_vector(x[1, 2, 2])),
    "place_lat3" = lapply(x$bounding_box$coordinates, function(x) prep_vector(x[1, 3, 2])),
    "place_lat4" = lapply(x$bounding_box$coordinates, function(x) prep_vector(x[1, 4, 2])))
  place_df
}

#' parse_user
#'
#' @param x json resposne object from user lookup Twitter API call.
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return data frame
#' @import dplyr
#' @export
parse_user <- function(x) {
  user_df <- data_frame(
    "user_id" = as.character(prep_vector(x$id_str)),
    "name" = as.character(prep_vector(x$name)),
    "screen_name" = as.character(prep_vector(x$screen_name)),
    "location" = as.character(prep_vector(x$location)),
    "description" = as.character(prep_vector(x$description)),
    "url" = as.character(prep_vector(x$url)),
    "entities_description_url" = prep_list(x$entities$description$urls, "expanded_url"),
    "entities_url" = prep_list(x$entities$url$urls, "expanded_url"),
    "protected" = as.logical(prep_vector(x$protected)),
    "followers_count" = as.integer(prep_vector(x$followers_count)),
    "friends_count" = as.integer(prep_vector(x$friends_count)),
    "listed_count" = as.integer(prep_vector(x$listed_count)),
    "created_at" = as.Date(as.POSIXct(as.character(prep_vector(x$created_at)),
                                      format="%a %b %d %H:%M:%S %z %Y"),
                           format = "%Y-%M-%D"),
    "favourites_count" = as.integer(prep_vector(x$favourites_count)),
    "utc_offset" = as.integer(prep_vector(x$utc_offset)),
    "time_zone" = as.character(prep_vector(x$time_zone)),
    "geo_enabled" = as.logical(prep_vector(x$geo_enabled)),
    "verified" = as.logical(prep_vector(x$verified)),
    "statuses_count" = as.integer(prep_vector(x$statuses_count)),
    "lang" = as.character(prep_vector(x$lang)))
  user_df
}

#' prep_list
#'
#' @param x data to be vectorized
#' @export
prep_list <- function(x, colname) {
  if (length(x) == 0) return(NA)
  x <- lapply(x, function(x) c(unlist(x[[`colname`]])))
  x[!unlist(lapply(x, is.null))] <- lapply(x[!unlist(lapply(x, is.null))], tolower)
  x[unlist(lapply(x, is.null))] <- NA
  x
}

#' prep_vector
#'
#' @param x data to be vectorized
#' @export
prep_vector <- function(x) {
  if (length(x) == 0) return(NA)
  x[x == ""] <- NA
  x[x == NaN] <- NA
  x[length(x) == 0] <- NA
  x[is.null(x)] <- NA
  as.vector(x)
}

#' parse_status_entities
#'
#' @param x statuses entities object
#' @export
parse_status_entities <- function(x) {
  status_entities_df <- data_frame(
    "entities_hashtag" = prep_list(x$hashtags, "text"),
    "entities_user_mentions_user_id" = prep_list(x$user_mentions, "id"),
    "entities_user_mentions_screen_name" = prep_list(x$user_mentions, "screen_name"),
    "entities_user_mentions_name" = prep_list(x$user_mentions, "name"),
    "entities_urls_expanded" = prep_list(x$urls, "expanded_url"),
    "entities_media_id" = prep_list(x$media, "id_str"),
    "entities_media_source_status_id" = prep_list(x$media, "source_status_id_str"),
    "entities_media_source_user_id" = prep_list(x$media, "source_user_id_str"),
    "entities_media_expanded_url" = prep_list(x$media, "expanded_url"))
  status_entities_df
}
