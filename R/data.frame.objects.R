#' data_frame_status
#'
#' @param x json object from search tweets
#' @import dplyr
#' @export
data_frame_status <- function(x) {
  status_df <- data_frame(
    "id" = prep_vector(as.character(x$id_str)),
    "text" = prep_vector(as.character(x$text)),
    "created_at" = as.Date(as.POSIXct(prep_vector(as.character(x$created_at)),
                                      format="%a %b %d %H:%M:%S %z %Y"),
                           format = "%Y-%M-%D"),
    "source" = prep_vector(as.character(x$source)),
    "in_reply_to_status_id" = prep_vector(as.character(x$in_reply_to_status_id_str)),
    "in_reply_to_user_id" = prep_vector(as.character(x$in_reply_to_user_id_str)),
    "in_reply_to_screen_name" = prep_vector(as.character(x$in_reply_to_screen_name)),
    "is_quote_status" = prep_vector(as.character(x$is_quote_status)),
    "retweet_count" = prep_vector(as.character(x$retweet_count)),
    "favorite_count" = prep_vector(as.character(x$favorite_count)),
    "favorited" = prep_vector(as.character(x$favorited)),
    "retweeted" = prep_vector(as.character(x$retweeted)),
    "lang" = prep_vector(as.character(x$lang)),
    "quoted_status_id" = prep_vector(as.character(x$quoted_status_id_str)),
    "urls" = try_catch(lapply(x$entities$urls, function(x) prep_vector(x$expanded_url))),
    "user_mentions" = try_catch(lapply(x$entities$user_mentions, function(x) prep_vector(x$id))),
    "hashtags" = try_catch(lapply(x$entities$hashtags, function(x) prep_vector(x[ , 1]))),
    "place_type" = try_catch(prep_vector(x$place$place_type)),
    "place_id" = try_catch(prep_vector(x$place$id)),
    "place_url" = try_catch(prep_vector(x$place$url)),
    "place_name" = try_catch(prep_vector(x$place$name)),
    "place_full_name" = try_catch(prep_vector(x$place$full_name)),
    "place_country_code" = try_catch(prep_vector(x$place$country_code)),
    "place_country" = try_catch(prep_vector(x$place$country)),
    "place_long1" = try_catch(lapply(x$place$bounding_box$coordinates, function(x) prep_vector(x[1, 1, 1]))),
    "place_long2" = try_catch(lapply(x$place$bounding_box$coordinates, function(x) prep_vector(x[1, 2, 1]))),
    "place_long3" = try_catch(lapply(x$place$bounding_box$coordinates, function(x) prep_vector(x[1, 3, 1]))),
    "place_long4" = try_catch(lapply(x$place$bounding_box$coordinates, function(x) prep_vector(x[1, 4, 1]))),
    "place_lat1" = try_catch(lapply(x$place$bounding_box$coordinates, function(x) prep_vector(x[1, 1, 2]))),
    "place_lat2" = try_catch(lapply(x$place$bounding_box$coordinates, function(x) prep_vector(x[1, 2, 2]))),
    "place_lat3" = try_catch(lapply(x$place$bounding_box$coordinates, function(x) prep_vector(x[1, 3, 2]))),
    "place_lat4" = try_catch(lapply(x$place$bounding_box$coordinates, function(x) prep_vector(x[1, 4, 2]))),
    "media_id" = try_catch(lapply(x$entities$media, function(x) prep_vector(x$id_str))),
    "media_url" = try_catch(lapply(x$entities$media, function(x) prep_vector(x$expanded_url))),
    "media_type" = try_catch(lapply(x$entities$media, function(x) prep_vector(x$type))))
  status_df
}



#' data_frame_lookup
#'
#' @param x json resposne object from user lookup Twitter API call.
#' @seealso See \url{https://dev.twitter.com/overview/documentation} for more information on using Twitter's API.
#' @return data frame
#' @import dplyr
#' @export
data_frame_lookup <- function(x) {
  user_df <- data_frame(
    "user_id" = prep_vector(as.character(x$id_str)),
    "name" = prep_vector(as.character(x$name)),
    "screen_name" = prep_vector(as.character(x$screen_name)),
    "location" = prep_vector(as.character(x$location)),
    "description" = prep_vector(as.character(x$description)),
    "url" = prep_vector(as.character(x$url)),
    "protected" = prep_vector(as.logical(x$protected)),
    "followers_count" = prep_vector(as.integer(x$followers_count)),
    "friends_count" = prep_vector(as.integer(x$friends_count)),
    "listed_count" = prep_vector(as.integer(x$listed_count)),
    "created_at" = as.Date(as.POSIXct(prep_vector(as.character(x$created_at)),
                                      format="%a %b %d %H:%M:%S %z %Y"),
                           format = "%Y-%M-%D"),
    "favourites_count" = prep_vector(as.integer(x$favourites_count)),
    "utc_offset" = prep_vector(as.integer(x$utc_offset)),
    "geo_enabled" = prep_vector(as.logical(x$geo_enabled)),
    "verified" = prep_vector(as.logical(x$verified)),
    "statuses_count" = prep_vector(as.integer(x$statuses_count)),
    "lang" = prep_vector(as.character(x$lang)))

  user_df
}

#' prep_vector
#'
#' @param x data to be vectorized
#' @export
prep_vector <- function(x) {
  if (length(x) == 0) return(NA)
  x[unlist(lapply(x, function(x) length(x) > 1))] <- unlist(
    lapply(x[unlist(lapply(x, function(x) length(x) > 1))], unlist))
  x[x == ""] <- NA
  x[x == NaN] <- NA
  x[length(x) == 0] <- NA
  x[is.null(x)] <- NA
  as.vector(x)
}
