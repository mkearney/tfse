#' search_tweets
#'
#' Returns a collection of relevant Tweets matching a specified query. Please
#' note that Twitter’s search service and, by extension, the Search API is not
#' meant to be an exhaustive source of Tweets. Not all Tweets will be indexed or
#' made available via the search interface. In API v1.1, the response format of
#' the Search API has been improved to return Tweet objects more similar to the
#' objects you’ll find across the REST API and platform. However, perspectival
#' attributes (fields that pertain to the perspective of the authenticating
#' user) are not currently supported on this endpoint. To learn how to use
#' Twitter Search effectively, consult our guide to Using the Twitter Search
#' API. See Working with Timelines to learn best practices for navigating
#' results by since_id and max_id.
#'
#' @param q required, A UTF-8, URL-encoded search query of 500 characters
#'   maximum, including operators. Queries may additionally be limited by
#'   complexity.
#' @param geocode optional, Returns tweets by users located within a given
#'   radius of the given latitude/longitude. The location is preferentially
#'   taking from the Geotagging API, but will fall back to their Twitter
#'   profile. The parameter value is specified by “latitude,longitude,radius”,
#'   where radius units must be specified as either “mi” (miles) or “km”
#'   (kilometers). Note that you cannot use the near operator via the API to
#'   geocode arbitrary locations; however you can use this geocode parameter to
#'   search near geocodes directly. A maximum of 1,000 distinct “sub-regions”
#'   will be considered when using the radius modifier.
#' @param lang optional, Restricts tweets to the given language, given by an ISO
#'   639-1 code. Language detection is best-effort.
#' @param locale optional, Specify the language of the query you are sending
#'   (only ja is currently effective). This is intended for language-specific
#'   consumers and the default should work in the majority of cases.
#' @param result_type optional, Specifies what type of search results you would
#'   prefer to receive. The current default is “mixed.” Valid values include
#'   \code{"mixed"} to include both popular and real time results in the
#'   response, \code{"recent"} to return only the most recent results in the
#'   response, and\code{"popular"} to return only the most popular results in
#'   the response.
#' @param count optional, The number of tweets to return per page, up to a
#'   maximum of 100. Defaults to 15. This was formerly the “rpp” parameter in
#'   the old Search API.
#' @param until optional, Returns tweets created before the given date. Date
#'   should be formatted as YYYY-MM-DD. Keep in mind that the search index has a
#'   7-day limit. In other words, no tweets will be found for a date older than
#'   one week. Example Values: \code{"2015-07-19"}.
#' @param since_id optional, Returns results with an ID greater than (that is,
#'   more recent than) the specified ID. There are limits to the number of
#'   Tweets which can be accessed through the API. If the limit of Tweets has
#'   occured since the since_id, the since_id will be forced to the oldest ID
#'   available.
#' @param max_id optional, Returns results with an ID less than (that is, older
#'   than) or equal to the specified ID.
#' @param include_entities optional, The entities node will be disincluded when
#'   set to false.
#' @param callback optional, If supplied, the response will use the JSONP format
#'   with a callback of the given name. The usefulness of this parameter is
#'   somewhat diminished by the requirement of authentication for requests to
#'   this endpoint. Example Values: \code{"processTweets"}
#' @param token OAuth token (1.0 or 2.0)
#' @seealso \url{https://api.twitter.com/1.1/search/tweets.json}
#' @return json object
#' @import dplyr
#' @export
search_tweets <- function(q, geocode = NULL, lang = NULL, locale = NULL,
                          result_type = "recent", count = 100, until = NULL, since_id = NULL,
                          max_id = NULL, include_entities = TRUE, token) {

  l <- vector("list", ceiling(count/100))

  params <- paste0("result_type=", result_type, "&count=", count,
                   if (!is.null(geocode)) paste0("&geocode=", geocode),
                   if (!is.null(lang)) paste0("&lang=", lang),
                   if (!is.null(locale)) paste0("&locale=", locale),
                   if (!is.null(until)) paste0("&until=", until),
                   if (!is.null(since_id)) paste0("&since_id=", since_id),
                   if (!is.null(max_id)) paste0("&max_id=", max_id),
                   if (include_entities) paste0("&include_entities=true") else paste0("&include_entities=false"))

  params <- paste0("q=", URLencode(q, reserved = TRUE),
                   "&", params)

  for (i in seq_along(l)) {
    out <- try_catch(TWIT(query = "search/tweets",
                          parameters = params,
                          token = token))

    if (length(out) == 1) return(NULL)

    tweets_df <- status_parse(out$statuses)

    l[[i]] <- tweets_df

    if (length(out$search_metadata$next_results) == 0) break

    params <- sub("[?]", "", out$search_metadata$next_results)
  }

  bind_rows(l)
}


#' status_parse
#'
#' @param x json object from search tweets
#' @import dplyr
#' @export
status_parse <- function(x) {
  status_df <- data_frame("id" = prep_vector(as.character(x$id_str)),
                          "text" = prep_vector(as.character(x$text)),
                          "created_at" = as.Date(as.POSIXct(prep_vector(as.character(x$created_at)),
                                                            format="%a %b %d %H:%M:%S %z %Y"), format = "%Y-%M-%D"),
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
                          "urls" = try_catch(lapply(x$entities$urls, function(x) list(prep_vector(x$expanded_url)))),
                          "user_mentions" = try_catch(lapply(x$entities$user_mentions, function(x) list(prep_vector(x$id)))),
                          "hashtags" = try_catch(lapply(x$entities$hashtags, function(x) list(prep_vector(x[ , 1])))),
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



#' top_tweet_words
#'
#' @param tweets_text character vector of tweets text
#' @param min minimum number of ocurrences to include in returned object
#' @return list object with top mentions and top words
#' @import dplyr
#' @export
top_tweet_words <- function(tweets_text, min = 3) {
  tweets_text <- unlist(lapply(tweets_text, function(x) gsub("\\n", " ", x)))
  tweets_text <- unlist(lapply(tweets_text, function(x) gsub("^[:alnum:]", "", x)  ))
  tweets_text <- unlist(strsplit(tweets_text, split = " "))

  mentions <- tweets_text[grep("@", tweets_text)]
  mentions <- tolower(mentions)
  mentions <- sort(table(mentions), decreasing = TRUE)

  words <- tweets_text[!1:length(tweets_text) %in% grep("@", tweets_text)]
  words <- words[!1:length(words) %in% grep("htt", words)]
  words <- words[!tolower(words) == "rt"]
  words <- gsub("'t", "t", gsub("'s", "", gsub('"', "", gsub("[#|...|.|,|:|;|-|_|—]", " ", words))))
  words <- unlist(strsplit(words, split = " "))
  words <- words[grep("[:digit:]", words)]
  words <- words[!words == ""]
  words <- words[nchar(words) > 2]
  words <- tolower(words)

  nowd <- c("a", "about", "after", "again", "all", "also", "and",
            "are", "at", "be", "been", "believe", "but", "instead",
            "can", "cant", "com", "could", "did", "do", "does", "done",
            "dont", "doesnt", "got", "most", "two", "didnt", "takes",
            "sent", "both", "guy", "gets", "get", "tweeted", "either",
            "gave", "get", "give", "go", "going", "had", "her", "him",
            "hillaryclinton", "hillaryclintons", "realdonaldtrumps",
            "his", "htt", "i", "if", "if", "in", "into", "is", "is",
            "it", "its", "just", "let", "my", "like", "make", "media",
            "need", "not", "on", "made", "take", "said", "taking",
            "tell", "there", "the", "isnt", "takes", "one", "org", "our",
            "out", "pretty", "put", "right", "goes", "have", "should",
            "says", "saying", "totally", "day", "first", "show", "shows",
            "still", "than", "that", "the", "their", "them", "then",
            "they", "theyre", "think", "this", "those", "time", "to",
            "too", "u", "via", "video", "want", "wants", "since", "days",
            "were", "what", "which", "will", "with", "would",
            "would", "www")

  words <- words[!words %in% nowd]
  words <- sort(table(words), decreasing = TRUE)

  list(words = words[words > min], mentions = mentions[mentions > min])
}
