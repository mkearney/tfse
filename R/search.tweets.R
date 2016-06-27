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
search_tweets <- function(q, geocode = NULL, lang = NULL, locale = NULL, result_type = "mixed", count = 100, until = NULL, since_id = NULL, max_id = NULL, include_entities = TRUE, token) {
  l <- vector("list", ceiling(count/100))

  params <- paste0("result_type=", result_type, "&count=", count,
                   if (!is.null(geocode)) paste0("&geocode=", geocode),
                   if (!is.null(lang)) paste0("&lang=", lang),
                   if (!is.null(locale)) paste0("&locale=", locale),
                   if (!is.null(until)) paste0("&until=", until),
                   if (!is.null(since_id)) paste0("&since_id=", since_id),
                   if (!is.null(max_id)) paste0("&max_id=", max_id),
                   if (include_entities) paste0("&include_entities=true") else paste0("&include_entities=false"))
  params <- paste0("q=",
                   paste(strsplit(q, " ")[[1]], collapse = "%20"),
                   "&",
                   params)

  for (i in seq_along(l)) {
    out <- TWIT(query = "search/tweets",
                parameters = params,
                token = token)
    params <- sub("[?]", "", out$search_metadata$next_results)
    if (is.null(out)) {
      l[[i]] <- NULL
    } else {
      out$statuses$entities$media <- lapply(out$statuses$entities$media, media_parse)
      entities <- bind_rows(out$statuses$entities$media)
      names(entities)[names(entities) %in% c("id", "id_str")] <- c("media_id", "media_id_str")
      out <- tbl_df(bind_rows(bind_rows(out$statuses[, unlist(lapply(out$statuses, is.vector))]), entities))
    out$created_at <- as.Date(as.POSIXct(out$created_at, format="%a %b %d %H:%M:%S %z %Y"), format = "%Y-%M-%D")
      l[[i]] <- out
    }
  }
  out <- bind_rows(l)

  out
}


#' media_parse
#'
#' @param x json object from search tweets
#' @import dplyr
#' @export
media_parse <- function(x) {
  media <- data_frame("id" = NA, "id_str" = NA, "indices" = NA, "media_url" = NA, "media_url_https" = NA, "url" = NA,
                      "display_url" = NA, "expanded_url" = NA, "type" = NA, "sizes" = NA, "source_status_id" = NA,
                      "source_status_id_str" = NA, "source_user_id" = NA, "source_user_id_str" = NA)
  if(is.null(x)) {
    x <- media
  } else {
    x <- bind_cols(x, media[names(media) %in% names(x)])
  }
  x
}
