#' get_statuses
#'
#' Returns a collection of the most recent Tweets posted by the user indicated
#' by the screen_name or user_id parameters. User timelines belonging to
#' protected users may only be requested when the authenticated user either
#' “owns” the timeline or is an approved follower of the owner. The timeline
#' returned is the equivalent of the one seen when you view a user’s profile on
#' twitter.com. This method can only return up to 3,200 of a user’s most recent
#' Tweets. Native retweets of other statuses by the user is included in this
#' total, regardless of whether include_rts is set to false when requesting this
#' resource.
#' @param user The screen_name or ID of the user for whom to return results for.
#' @param since_id Returns results with an ID greater than (that is, more recent
#'   than) the specified ID. There are limits to the number of Tweets which can
#'   be accessed through the API. If the limit of Tweets has occured since the
#'   since_id, the since_id will be forced to the oldest ID available.
#' @param count Specifies the number of tweets to try and retrieve, up to a
#'   maximum of 200 per distinct request. The value of count is best thought of
#'   as a limit to the number of tweets to return because suspended or deleted
#'   content is removed after the count has been applied. We include retweets in
#'   the count, even if include_rts is not supplied. It is recommended you
#'   always send include_rts=1 when using this API method.
#' @param max_id Returns results with an ID less than (that is, older than) or
#'   equal to the specified ID.
#' @param trim_user When set to either true, t or 1, each tweet returned in a
#'   timeline will include a user object including only the status authors
#'   numerical ID. Omit this parameter to receive the complete user object.
#' @param exclude_replies This parameter will prevent replies from appearing in
#'   the returned timeline. Using exclude_replies with the count parameter will
#'   mean you will receive up-to count tweets — this is because the count
#'   parameter retrieves that many tweets before filtering out retweets and
#'   replies. This parameter is only supported for JSON and XML responses.
#' @param contributor_details This parameter enhances the contributors element
#'   of the status response to include the screen_name of the contributor. By
#'   default only the user_id of the contributor is included.
#' @param include_rts When set to false, the timeline will strip any native
#'   retweets (though they will still count toward both the maximal length of
#'   the timeline and the slice selected by the count parameter). Note: If
#'   you’re using the trim_user parameter in conjunction with include_rts, the
#'   retweets will still contain a full user object.
#' @param token OAuth token (1.0 or 2.0)
#' @seealso See \url{https://api.twitter.com/1.1/statuses/user_timeline.json}.
#' @return json object (nested list)
#' @export
get_friendships <- function(user, since_id = NULL, count = NULL, max_id = NULL,
                            trim_user = NULL, exclude_replies = NULL,
                            contributer_details = NULL, include_rts = NULL,
                            token) {

  if (is_screen_name(user)) {
    id_type <- "screen_name"
  } else {
    id_type <- "id"
  }

  if (!is.null(since_id)) {
    since_id <- paste0("&since_id=", since_id)
  }

  if (!is.null(count)) {
    count <- paste0("&count=", count)
  }

  if (!is.null(max_id)) {
    max_id <- paste0("&max_id=", max_id)
  }

  if (!is.null(trim_user)) {
    trim_user <- paste0("&trim_user=", trim_user)
  }

  if (!is.null(exclude_replies)) {
    exclude_replies <- paste0("&exclude_replies=", exclude_replies)
  }

  if (!is.null(contributer_details)) {
    contributer_details <- paste0("&contributer_details=", contributer_details)
  }

  if (!is.null(include_rts)) {
    include_rts <- paste0("&include_rts=", include_rts)
  }
  params <- paste0(since_id, count, max_id,
                   trim_user, exclude_replies,
                   contributer_details, include_rts)

  out <- TWIT(query = "statuses/user_timeline",
              parameters = paste0(id_type, "=", user, params),
              token = token)

  return(out)
}

#' get_statuses_retweets
#'
#' Returns a collection of the 100 most recent retweets of the tweet specified
#' by the id parameter.
#' @param tweet_id required, The numerical ID of the desired status
#' @param count optional, Specifies the number of records to retrieve. Must be
#'   less than or equal to 100.
#' @param trim_user optional, When set to TRUE each tweet returned in a timeline
#'   will include a user object including only the status authors numerical ID.
#'   Omit this parameter to receive the complete user object.
#' @param token OAuth token (1.0 or 2.0)
#' @seealso \url{https://api.twitter.com/1.1/statuses/retweets/:id.json}
#' @return json object
get_statuses_retweets <- function(tweet_id, count = 100, trim_user = TRUE, token) {

  if (trim_user) {
    params <- paste0("count=", count, "&trim_user=true")
  } else {
    params <- paste0("count=", count)
  }

  out <- TWIT(query = "statuses/retweets",
              parameters = paste0("id=", tweet_id, "&", params),
              token = token)

  return(out)
}
